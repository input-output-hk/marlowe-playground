module Marlowe.Holes.SemanticTest where

import Prologue

import Control.Monad.Error.Class (class MonadError)
import Data.BigInt.Argonaut (BigInt, fromInt)
import Data.DateTime.Instant (Instant)
import Data.Foldable (for_)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (maybe')
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Error)
import Examples.PureScript.ContractForDifferences as ContractForDifferences
import Examples.PureScript.Escrow as Escrow
import Language.Marlowe.Core.V1.Semantics (computeTransaction) as S
import Language.Marlowe.Core.V1.Semantics (emptyState)
import Language.Marlowe.Core.V1.Semantics.Types
  ( ChoiceId(..)
  , Contract
  , InputContent(..)
  , TimeInterval(..)
  , TransactionOutput(..)
  ) as S
import Language.Marlowe.Core.V1.Semantics.Types (Party(..), Token(..))
import Language.Marlowe.Extended.V1 (toCore)
import Language.Marlowe.Extended.V1 as EM
import Language.Marlowe.ToTerm (toTerm)
import Marlowe.Holes
  ( Term
  , TransactionInputContent(..)
  , fromTerm
  , toTransactionInput
  )
import Marlowe.Holes as T
import Marlowe.Template (TemplateContent(..), fillTemplate)
import Marlowe.Time (unixEpoch, unsafeInstantFromInt)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

-- For the purposes of this test all transactions can happen in slot 0
transaction :: S.InputContent -> TransactionInputContent
transaction input =
  TransactionInputContent
    { interval: S.TimeInterval unixEpoch unixEpoch
    , inputs: List.singleton input
    }

multipleInputs :: List S.InputContent -> TransactionInputContent
multipleInputs inputs =
  TransactionInputContent
    { interval: S.TimeInterval unixEpoch unixEpoch
    , inputs: inputs
    }

timeout :: Instant -> TransactionInputContent
timeout instant =
  TransactionInputContent
    { interval: S.TimeInterval instant instant
    , inputs: mempty
    }

type ContractFlows = List (String /\ List TransactionInputContent)

------------------------------------------------------------------------------------------------------
seller :: Party
seller = Role "Seller"

buyer :: Party
buyer = Role "Buyer"

arbiter :: Party
arbiter = Role "Mediator"

lovelace :: Token
lovelace = Token "" ""

escrowPrice :: BigInt
escrowPrice = fromInt 1500

escrowContract :: EM.Contract
escrowContract =
  fillTemplate
    ( TemplateContent
        { timeContent: Map.empty
        , valueContent:
            Map.fromFoldable
              [ "Price" /\ escrowPrice
              ]
        }
    )
    Escrow.fixedTimeoutContract

escrowFlows :: ContractFlows
escrowFlows =
  List.fromFoldable
    [ "Everything is alright"
        /\ List.fromFoldable
          [ transaction $ S.IDeposit seller buyer lovelace escrowPrice
          , transaction $ S.IChoice (S.ChoiceId "Everything is alright" buyer)
              zero
          ]
    , "Seller confirm problem"
        /\ List.fromFoldable
          [ transaction $ S.IDeposit seller buyer lovelace escrowPrice
          , transaction $ S.IChoice (S.ChoiceId "Report problem" buyer) one
          , transaction $ S.IChoice (S.ChoiceId "Confirm problem" seller) one
          ]
    , "Dismiss claim"
        /\ List.fromFoldable
          [ transaction $ S.IDeposit seller buyer lovelace escrowPrice
          , transaction $ S.IChoice (S.ChoiceId "Report problem" buyer) one
          , transaction $ S.IChoice (S.ChoiceId "Dispute problem" seller) zero
          , transaction $ S.IChoice (S.ChoiceId "Dismiss claim" arbiter) zero
          ]
    , "Mediator confirm problem"
        /\ List.fromFoldable
          [ transaction $ S.IDeposit seller buyer lovelace escrowPrice
          , transaction $ S.IChoice (S.ChoiceId "Report problem" buyer) one
          , transaction $ S.IChoice (S.ChoiceId "Dispute problem" seller) zero
          , transaction $ S.IChoice (S.ChoiceId "Confirm problem" arbiter) one
          ]
    , "Mediator confirm problem (multiple actions in same transaction)"
        /\ List.singleton
          ( multipleInputs
              $ List.fromFoldable
                  [ S.IDeposit seller buyer lovelace escrowPrice
                  , S.IChoice (S.ChoiceId "Report problem" buyer) one
                  , S.IChoice (S.ChoiceId "Dispute problem" seller) zero
                  , S.IChoice (S.ChoiceId "Confirm problem" arbiter) one
                  ]
          )
    , "Invalid input"
        /\ List.fromFoldable
          [ transaction $ S.IDeposit arbiter arbiter lovelace escrowPrice
          ]
    , "Timeout 1"
        /\ List.fromFoldable
          [ timeout (unsafeInstantFromInt 61)
          ]
    , "Timeout 2"
        /\ List.fromFoldable
          [ transaction $ S.IDeposit seller buyer lovelace escrowPrice
          , timeout (unsafeInstantFromInt 181)
          ]
    -- Because the slot 10 is lower than the first timeout (60), this "timeout" should
    -- not be significant
    , "Everything is alright (with non significant timeout)"
        /\ List.fromFoldable
          [ timeout (unsafeInstantFromInt 10)
          , transaction $ S.IDeposit seller buyer lovelace escrowPrice
          , transaction $ S.IChoice (S.ChoiceId "Everything is alright" buyer)
              zero
          ]
    ]

------------------------------------------------------------------------------------------------------
party :: Party
party = Role "Party"

counterparty :: Party
counterparty = Role "Counterparty"

oracle :: Party
oracle = Role "Oracle"

cfdPrice :: BigInt
cfdPrice = fromInt 100000000

contractForDifferences :: EM.Contract
contractForDifferences =
  fillTemplate
    ( TemplateContent
        { timeContent:
            Map.fromFoldable
              [ "Party deposit deadline" /\ unsafeInstantFromInt 10
              , "Counterparty deposit deadline" /\ unsafeInstantFromInt 20
              , "First window beginning" /\ unsafeInstantFromInt 30
              , "First window deadline" /\ unsafeInstantFromInt 40
              , "Second window beginning" /\ unsafeInstantFromInt 100
              , "Second window deadline" /\ unsafeInstantFromInt 110
              ]
        , valueContent:
            Map.fromFoldable
              [ "Amount paid by party" /\ fromInt 100000000
              , "Amount paid by counterparty" /\ fromInt 100000000
              ]
        }
    )
    ContractForDifferences.contract

contractForDifferencesFlows :: ContractFlows
contractForDifferencesFlows =
  List.fromFoldable
    [ "Decrease in price"
        /\ List.fromFoldable
          [ transaction $ S.IDeposit party party lovelace cfdPrice
          , transaction $ S.IDeposit counterparty counterparty lovelace cfdPrice
          , timeout (unsafeInstantFromInt 35)
          , transaction $ S.IChoice (S.ChoiceId "Price in first window" oracle)
              (fromInt 90000000)
          , timeout (unsafeInstantFromInt 105)
          , transaction $ S.IChoice (S.ChoiceId "Price in second window" oracle)
              (fromInt 85000000)
          ]
    , "Increase in price"
        /\ List.fromFoldable
          [ transaction $ S.IDeposit party party lovelace cfdPrice
          , transaction $ S.IDeposit counterparty counterparty lovelace cfdPrice
          , timeout (unsafeInstantFromInt 35)
          , transaction $ S.IChoice (S.ChoiceId "Price in first window" oracle)
              (fromInt 90000000)
          , timeout (unsafeInstantFromInt 105)
          , transaction $ S.IChoice (S.ChoiceId "Price in second window" oracle)
              (fromInt 95000000)
          ]
    , "Same price"
        /\ List.fromFoldable
          [ transaction $ S.IDeposit party party lovelace cfdPrice
          , transaction $ S.IDeposit counterparty counterparty lovelace cfdPrice
          , timeout (unsafeInstantFromInt 35)
          , transaction $ S.IChoice (S.ChoiceId "Price in first window" oracle)
              (fromInt 90000000)
          , timeout (unsafeInstantFromInt 105)
          , transaction $ S.IChoice (S.ChoiceId "Price in second window" oracle)
              (fromInt 90000000)
          ]
    ]

------------------------------------------------------------------------------------------------------

extendedToSemanticAndTerm
  :: EM.Contract -> Maybe (S.Contract /\ Term T.Contract)
extendedToSemanticAndTerm extendedContract = do
  semanticContract <- toCore extendedContract
  let
    termContract = toTerm extendedContract
  pure $ semanticContract /\ termContract

shouldHaveSameOutput
  :: forall m
   . MonadError Error m
  => S.TransactionOutput
  -> T.TransactionOutput
  -> m Unit
shouldHaveSameOutput (S.TransactionOutput o1) (T.TransactionOutput o2) = do
  shouldEqual o1.txOutWarnings o2.txOutWarnings
  shouldEqual o1.txOutPayments o2.txOutPayments
  shouldEqual o1.txOutState o2.txOutState
  shouldEqual (Just o1.txOutContract) (fromTerm o2.txOutContract)

shouldHaveSameOutput (S.Error e1) (T.SemanticError e2) = shouldEqual e1 e2

shouldHaveSameOutput _ T.InvalidContract = fail "The contract is invalid"

shouldHaveSameOutput _ _ = fail "The outputs don't match"

-- initialSlot :: S.Contract -> Slot
-- initialSlot = maybe zero (\(Slot slot) -> Slot (slot - fromInt 1)) <<< _.minTime <<< unwrap <<< timeouts
-- This function test that given an extended contract with a list of transactions, the
-- result of computing the transaction list is the same for the Semantic and the Term version
testTransactionList
  :: forall m
   . MonadError Error m
  => EM.Contract
  -> List TransactionInputContent
  -> m Unit
testTransactionList extendedContract inputs =
  maybe'
    (\_ -> fail "could not instantiate contract")
    testAllInputs
    (extendedToSemanticAndTerm extendedContract)
  where
  testAllInputs (semanticContract /\ termContract) =
    let
      state = emptyState
    in
      testStep inputs state semanticContract termContract

  testStep Nil _ semanticContract termContract = shouldEqual
    (Just semanticContract)
    (fromTerm termContract)

  testStep (input : rest) state semanticContract termContract = do
    let
      input' = toTransactionInput input
      semanticOutput = S.computeTransaction input' state semanticContract

      termOutput = T.computeTransaction input state termContract
    shouldHaveSameOutput semanticOutput termOutput
    case semanticOutput /\ termOutput of
      S.TransactionOutput o1 /\ T.TransactionOutput o2 -> testStep rest
        o1.txOutState
        o1.txOutContract
        o2.txOutContract
      _ -> pure unit

all :: Spec Unit
all =
  describe "Marlowe.Holes.Semantic" do
    describe "Escrow flows" do
      for_ escrowFlows \(name /\ flow) ->
        it name $ testTransactionList escrowContract flow
    describe "Contract for differences" do
      for_ contractForDifferencesFlows \(name /\ flow) ->
        it name $ testTransactionList contractForDifferences flow
