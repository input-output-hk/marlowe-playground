module StaticAnalysis.Reachability
  ( analyseReachability
  , areContractAndStateTheOnesAnalysed
  , getUnreachableContracts
  , startReachabilityAnalysis
  ) where

import Prologue hiding (div)

import Data.Lens (assign, use)
import Data.List (List(..))
import Data.List.NonEmpty (toList)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff.Class (class MonadAff)
import Halogen (HalogenM)
import Language.Marlowe.Core.V1.Semantics (emptyState)
import Language.Marlowe.Core.V1.Semantics.Types (Contract(..), Observation(..))
import Language.Marlowe.Core.V1.Semantics.Types as S
import Language.Marlowe.Extended.V1 (toCore)
import Language.Marlowe.Extended.V1 as EM
import Marlowe (Api)
import Marlowe.Linter (hasInvalidAddresses)
import Marlowe.Template (fillTemplate)
import Servant.PureScript (class MonadAjax)
import StaticAnalysis.StaticTools
  ( closeZipperContract
  , startMultiStageAnalysis
  , zipperToContractPath
  )
import StaticAnalysis.Types
  ( AnalysisExecutionState(..)
  , AnalysisState
  , ContractPath
  , ContractZipper
  , MultiStageAnalysisData(..)
  , MultiStageAnalysisProblemDef
  , _analysisExecutionState
  , _analysisState
  , _templateContent
  )

analyseReachability
  :: forall m state action slots
   . MonadAff m
  => MonadAjax Api m
  => EM.Contract
  -> HalogenM { analysisState :: AnalysisState | state } action slots Void m
       Unit
analyseReachability extendedContract = do
  templateContent <- use (_analysisState <<< _templateContent)
  if hasInvalidAddresses extendedContract then
    assign (_analysisState <<< _analysisExecutionState)
      $ CloseAnalysis
      $ AnalysisFailure
          "The code has invalid addresses. Please check the Warnings tab."
  else
    case toCore $ fillTemplate templateContent extendedContract of
      Just contract -> do
        assign (_analysisState <<< _analysisExecutionState)
          (ReachabilityAnalysis AnalysisNotStarted)
        -- when editor and simulator were together the analyse contract could be made
        -- at any step of the simulator. Now that they are separate, it can only be done
        -- with initial state
        let
          emptySemanticState = emptyState
        newReachabilityAnalysisState <- startReachabilityAnalysis contract
          emptySemanticState
        assign (_analysisState <<< _analysisExecutionState)
          (ReachabilityAnalysis newReachabilityAnalysisState)
      Nothing -> assign (_analysisState <<< _analysisExecutionState)
        ( ReachabilityAnalysis $ AnalysisFailure
            "The code has templates. Static analysis can only be run in core Marlowe code."
        )

expandSubproblem :: ContractZipper -> Contract -> (ContractPath /\ Contract)
expandSubproblem z _ = zipperToContractPath z /\ closeZipperContract z
  (Assert FalseObs Close)

isValidSubproblem :: ContractZipper -> Contract -> Boolean
isValidSubproblem _ _ = true

reachabilityAnalysisDef :: MultiStageAnalysisProblemDef
reachabilityAnalysisDef =
  { analysisDataSetter: ReachabilityAnalysis
  , expandSubproblemImpl: expandSubproblem
  , isValidSubproblemImpl: isValidSubproblem
  , shouldExamineChildren: identity
  , isProblemCounterExample: identity
  }

startReachabilityAnalysis
  :: forall m state action slots
   . MonadAff m
  => MonadAjax Api m
  => Contract
  -> S.State
  -> HalogenM { analysisState :: AnalysisState | state } action slots Void m
       MultiStageAnalysisData
startReachabilityAnalysis = startMultiStageAnalysis reachabilityAnalysisDef

getUnreachableContracts :: AnalysisExecutionState -> List ContractPath
getUnreachableContracts (ReachabilityAnalysis (AnalysisInProgress ipr)) =
  ipr.counterExampleSubcontracts

getUnreachableContracts (ReachabilityAnalysis (AnalysisFoundCounterExamples us)) =
  toList us.counterExampleSubcontracts

getUnreachableContracts _ = Nil

areContractAndStateTheOnesAnalysed
  :: AnalysisExecutionState -> Maybe Contract -> S.State -> Boolean
areContractAndStateTheOnesAnalysed
  (ReachabilityAnalysis (AnalysisInProgress ipr))
  (Just contract)
  state = ipr.originalContract == contract && ipr.originalState == state

areContractAndStateTheOnesAnalysed
  (ReachabilityAnalysis (AnalysisFoundCounterExamples us))
  (Just contract)
  state = us.originalContract == contract && us.originalState == state

areContractAndStateTheOnesAnalysed _ _ _ = false

