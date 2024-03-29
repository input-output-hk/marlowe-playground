module CloseAnalysis where

import Prologue hiding (div)

import Data.Foldable (foldl)
import Data.Lens (assign, use)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff.Class (class MonadAff)
import Halogen (HalogenM)
import Language.Marlowe.Core.V1.Semantics (emptyState)
import Language.Marlowe.Core.V1.Semantics.Types
  ( AccountId
  , Contract(..)
  , Observation(..)
  , Payee(..)
  , Token
  , Value(..)
  )
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
  , ContractZipper(..)
  , MultiStageAnalysisData(..)
  , MultiStageAnalysisProblemDef
  , _analysisExecutionState
  , _analysisState
  , _templateContent
  )

analyseClose
  :: forall m state action slots
   . MonadAff m
  => MonadAjax Api m
  => EM.Contract
  -> HalogenM { analysisState :: AnalysisState | state } action slots Void m
       Unit
analyseClose extendedContract = do
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
          (CloseAnalysis AnalysisNotStarted)
        -- when editor and simulator were together the analyse contract could be made
        -- at any step of the simulator. Now that they are separate, it can only be done
        -- with initial state
        let
          emptySemanticState = emptyState
        newCloseAnalysisState <- startCloseAnalysis contract emptySemanticState
        assign (_analysisState <<< _analysisExecutionState)
          (CloseAnalysis newCloseAnalysisState)
      Nothing -> assign (_analysisState <<< _analysisExecutionState)
        ( CloseAnalysis $ AnalysisFailure
            "The code has templates. Static analysis can only be run in core Marlowe code."
        )

extractAccountIdsFromZipper :: ContractZipper -> Set (AccountId /\ Token)
extractAccountIdsFromZipper = go
  where
  go (PayZip _ (Account accountId) token _ contZip) =
    Set.insert (accountId /\ token) $ go contZip

  go (PayZip _ _ _ _ contZip) = go contZip

  go (WhenCaseZip _ (S.Deposit accountId _ token _) contZip _ _ _) =
    Set.insert (accountId /\ token) $ go contZip

  go (WhenCaseZip _ _ contZip _ _ _) = go contZip

  go (IfTrueZip _ contZip _) = go contZip

  go (IfFalseZip _ _ contZip) = go contZip

  go (WhenTimeoutZip _ _ contZip) = go contZip

  go (LetZip _ _ contZip) = go contZip

  go (AssertZip _ contZip) = go contZip

  go HeadZip = mempty

addAssertionForAccountId :: Contract -> (AccountId /\ Token) -> Contract
addAssertionForAccountId cont (accountId /\ token) = Assert
  (ValueEQ (AvailableMoney accountId token) (Constant zero))
  cont

-- We expect "contract" to be Close always, but we take it as a parameter anyway because it makes more sense
expandSubproblem :: ContractZipper -> Contract -> (ContractPath /\ Contract)
expandSubproblem zipper contract = zipperToContractPath zipper /\
  closeZipperContract zipper modifiedContract
  where
  accountIds = extractAccountIdsFromZipper zipper

  modifiedContract = foldl addAssertionForAccountId contract accountIds

isValidSubproblem :: ContractZipper -> Contract -> Boolean
isValidSubproblem _ Close = true

isValidSubproblem _ _ = false

closeAnalysisAnalysisDef :: MultiStageAnalysisProblemDef
closeAnalysisAnalysisDef =
  { analysisDataSetter: CloseAnalysis
  , expandSubproblemImpl: expandSubproblem
  , isValidSubproblemImpl: isValidSubproblem
  , shouldExamineChildren: const true
  , isProblemCounterExample: not
  }

startCloseAnalysis
  :: forall m state action slots
   . MonadAff m
  => MonadAjax Api m
  => Contract
  -> S.State
  -> HalogenM { analysisState :: AnalysisState | state } action slots Void m
       MultiStageAnalysisData
startCloseAnalysis = startMultiStageAnalysis closeAnalysisAnalysisDef
