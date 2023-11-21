{-# LANGUAGE DeriveGeneric #-}
module Marlowe.Symbolic.Types.Response where

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)
import Language.Marlowe.Core.V1.Semantics (TransactionInput, TransactionWarning)

newtype POSIXTimeWrapper = POSIXTimeWrapper Integer
  deriving (Generic)

instance ToJSON POSIXTimeWrapper
instance FromJSON POSIXTimeWrapper

data Result = Valid
            | CounterExample
                { initialTime        :: POSIXTimeWrapper
                , transactionList    :: [TransactionInput]
                , transactionWarning :: [TransactionWarning]
                }
            | Error String
  deriving (Generic)

instance FromJSON Result
instance ToJSON Result

data Response = Response { result     :: Result
                         , durationMs :: Integer
                         }
  deriving (Generic)

instance FromJSON Response
instance ToJSON Response


