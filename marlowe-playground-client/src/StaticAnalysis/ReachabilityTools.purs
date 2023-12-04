module StaticAnalysis.ReachabilityTools
  ( initializePrefixMap
  , stepPrefixMap
  ) where

import Prologue hiding (div)

import Control.Monad.State as CMS
import Data.List (List, any, catMaybes, fromFoldable, null)
import Data.List.NonEmpty (fromList, head, tail)
import Data.Map (fromFoldableWith, lookup, unionWith)
import Data.Map as Map
import Data.Set (singleton, union)
import Data.Tuple.Nested ((/\))
import StaticAnalysis.Types (ContractPath, ContractPathStep, PrefixMap)

-- It groups the contract paths by their head, discards empty contract paths
initializePrefixMap :: List ContractPath -> PrefixMap
initializePrefixMap unreachablePathList = fromFoldableWith union
  $ map (\x -> (head x /\ singleton x))
  $ catMaybes
  $ map fromList unreachablePathList

-- Returns Nothing when the path is unreachable according to one of the paths, otherwise it returns the updated PrefixMap for the subpath
stepPrefixMap
  :: forall a
   . CMS.State a Unit
  -> PrefixMap
  -> ContractPathStep
  -> CMS.State a (Maybe PrefixMap)
stepPrefixMap markUnreachable prefixMap contractPath =
  case lookup contractPath prefixMap of
    Just pathSet ->
      let
        tails = map tail $ fromFoldable pathSet
      in
        if any null tails then do
          markUnreachable
          pure Nothing
        else
          pure $ Just $ unionWith union (initializePrefixMap tails) Map.empty
    Nothing -> pure (Just Map.empty)