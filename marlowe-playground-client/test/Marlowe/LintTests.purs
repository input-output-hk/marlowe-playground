module Marlowe.LintTests where

import Prologue

import Control.Monad.Error.Class (class MonadThrow)
import Data.Array (singleton)
import Data.List (List(..), fold)
import Data.List.Lazy (replicate)
import Data.Map as Map
import Data.Set (toUnfoldable)
import Data.String.CodeUnits as S
import Data.Traversable (class Foldable, foldMap, for_)
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Error)
import Marlowe.Linter (State(..), WarningDetail(..), lint)
import Marlowe.Parser (parseContract)
import StaticData (marloweContracts)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

all :: Spec Unit
all = do
  describe "Marlowe.Linter " do
    it "reports simplification in Let construct" letSimplifies
    it "reports simplication in Deposit construct" depositSimplifies
    it "reports simplication in Pay construct" paySimplifies
    it "reports simplication in AddValue" addValueSimplifies
    it
      "reports simplication of AddValue with 0 constant"
      addValueSimplifiesWithZero
    it "reports simplication in SubValue" subValueSimplifies
    it
      "reports simplication of SubValue with 0 constant"
      subValueSimplifiesWithZero
    it "reports simplication in If construct" ifSimplifies
    it "reports simplication in Notify construct" notifySimplifies
    it "reports simplication in Assert construct" assertSimplifies
    it "reports simplication in AndObs" andObsSimplifies
    it
      "reports simplication of AndObs with True constant"
      andObsSimplifiesWithTrue
    it "reports simplication in OrObs" orObsSimplifies
    it
      "reports simplication of OrObs with False constant"
      orObsSimplifiesWithFalse
    it "reports simplication of DivValue 0 / _" divZeroSimplified
    it "reports simplication of DivValue by 0" divByZeroSimplified
    it "reports simplication of DivValue with constant" divConstantSimplified
    it "reports simplication Invalid bound in Case" unreachableCaseInvalidBound
    it "reports bad practices Let shadowing" letShadowing
    it "reports role name too many characters" roleTooLongWarning
    it "reports role name too many bytes" roleTooLongBytesWarning
    it "does not report 32 ANSI-characters role name" roleOkNoWarning
    it "does not report 32 bytes role name" roleOkBytesNoWarning
    it "reports currency symbol is too short" currencySymbolTooShortWarning
    it "reports currency symbol is too long" currencySymbolTooLongWarning
    it "does not report currency symbol is the right length"
      currencySymbolOkNoWarning
    it "reports token name too many characters" tokenNameTooLongWarning
    it "reports token name too many bytes" tokenNameTooLongBytesWarning
    it "does not report 32 ANSI-characters token name" tokenNameOkNoWarning
    it "does not report 32 bytes token name" tokenNameOkBytesNoWarning
    it "reports contract with addresses from different network"
      networkMismatchWarning
    it "does not report contract with only addresses from mainnet"
      mainnetNetworkNoWarning
    it "does not report contract with only addresses from testnet"
      testnetNetworkNoWarning
    it "reports bad practices Non-increasing timeouts" nonIncreasingTimeouts
    it "reports unreachable code Unreachable If branch (then)" unreachableThen
    it "reports unreachable code Unreachable If branch (else)" unreachableElse
    it
      "reports unreachable code Unreachable Case (Notify)"
      unreachableCaseNotify
    it
      "reports unreachable code Unreachable Case (empty Choice list)"
      unreachableCaseEmptyChoiceList
    it "reports bad contracts Undefined Let" undefinedLet
    it "reports bad contracts Undefined ChoiceValue" undefinedChoiceValue
    it "reports bad contracts Non-positive Deposit" nonPositiveDeposit
    it "reports bad contracts Non-positive Pay" nonPositivePay
    it "reports bad contracts Pay before deposit" payBeforeWarning
    it
      "reports bad contracts Pay before deposit in branch"
      payBeforeWarningBranch
    it
      "reports bad contracts Pay with insufficient deposit"
      payInsufficientDeposit
    it
      "reports bad contracts Pay twice with insufficient deposit for both"
      payTwiceInsufficientDeposit
    it "does not report good contracts Defined Let" normalLet
    it "does not report good contracts Defined ChoiceValue" normalChoiceValue
    it "does not report good contracts Positive Deposit" positiveDeposit
    it "does not report good contracts Positive Pay" positivePay
    it "does not report good contracts Pay to hole" payToHole
    it
      "does not report good contracts Pay to account and then Pay"
      payThroughAccount
    it "does not report good contracts Pay twice" payTwice
    let
      examples :: Array (String /\ String)
      examples = Map.toUnfoldable marloweContracts
    for_ examples \(name /\ contract) ->
      it (show name <> " example passes Marlowe.Linter") do
        testNoWarning contract

addParenthesis :: String -> String
addParenthesis str = "(" <> str <> ")"

letContract :: String -> String
letContract subExpression = "Let \"simplifiableValue\" " <> subExpression <>
  " Close"

addContract :: String -> String
addContract subExpression =
  "Let \"simplifiableValue\" (AddValue TimeIntervalEnd " <> subExpression <>
    ") Close"

subContract :: String -> String
subContract subExpression =
  "Let \"simplifiableValue\" (SubValue TimeIntervalEnd " <> subExpression <>
    ") Close"

depositAndThenDo :: String -> String -> String
depositAndThenDo subExpression continuation =
  "When [Case (Deposit (Role \"role\") (Role \"role\") (Token \"\" \"\") "
    <> subExpression
    <> ") "
    <> continuation
    <> "] 10 Close"

depositContract :: String -> String
depositContract subExpression = depositAndThenDo subExpression "Close"

choiceAndThenDo :: String -> String
choiceAndThenDo continuation =
  "When [Case (Choice (ChoiceId \"choice\" (Role \"role\")) [Bound 50 100]) "
    <> continuation
    <> "] 5 Close"

payContract :: String -> String -> String
payContract party subExpression =
  "When [Case (Deposit (" <> party <> ") (" <> party
    <> ") (Token \"\" \"\") (Constant 100)) (Pay ("
    <> party
    <> ") (Party ("
    <> party
    <> ")) (Token \"\" \"\") "
    <> subExpression
    <> " Close)] 10 Close"

payContractWithRole :: String -> String
payContractWithRole = payContract "Role \"role\""

payContractWithPaymentKeyAddress :: String -> String
payContractWithPaymentKeyAddress = payContract
  "Address \"addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3n0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgse35a3x\""

invalidAddress :: String
invalidAddress =
  "addr1z8phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gten0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgs9yc0hh"

payContractWithScriptAddress :: String -> String
payContractWithScriptAddress = payContract "Address \" <> invalidAddress <> \""

notifyContract :: String -> String
notifyContract subExpression = "When [Case (Notify " <> subExpression <>
  ") Close] 10 Close"

assertContract :: String -> String
assertContract subExpression = "Assert " <> subExpression <> " Close"

ifContract :: String -> String
ifContract subExpression = "If " <> subExpression <> " Close Close"

andContract :: String -> String
andContract subExpression =
  "If (AndObs (ValueLE TimeIntervalEnd (Constant 20)) " <> subExpression <>
    ") Close Close"

orContract :: String -> String
orContract subExpression = "If (OrObs (ValueGE TimeIntervalEnd (Constant 5)) "
  <> subExpression
  <> ") Close Close"

makeValueSimplificationWarning :: String -> String -> String
makeValueSimplificationWarning simplifiableExpression simplification =
  "The value \"" <> simplifiableExpression <> "\" can be simplified to \""
    <> simplification
    <> "\""

makeObservationSimplificationWarning :: String -> String -> String
makeObservationSimplificationWarning simplifiableExpression simplification =
  "The observation \"" <> simplifiableExpression <> "\" can be simplified to \""
    <> simplification
    <> "\""

testWarning
  :: forall m a
   . MonadThrow Error m
  => (a -> Array String)
  -> (a -> String)
  -> a
  -> m Unit
testWarning makeWarning composeExpression expression =
  case parseContract $ composeExpression expression of
    Right contractTerm -> do
      let
        State st = lint Nil contractTerm
      shouldEqual
        (makeWarning expression)
        $ map show
        $ toUnfoldable
        $ st.warnings
    Left err -> fail (show err)

testSimplificationWarning
  :: forall m a b
   . MonadThrow Error m
  => (a -> b -> String)
  -> (a -> String)
  -> a
  -> b
  -> m Unit
testSimplificationWarning f g simplifiableExpression simplification =
  testWarning (singleton <<< uncurry f) (g <<< fst)
    (simplifiableExpression /\ simplification)

testValueSimplificationWarning
  :: forall m
   . MonadThrow Error m
  => (String -> String)
  -> String
  -> String
  -> m Unit
testValueSimplificationWarning = testSimplificationWarning
  makeValueSimplificationWarning

testObservationSimplificationWarning
  :: forall m
   . MonadThrow Error m
  => (String -> String)
  -> String
  -> String
  -> m Unit
testObservationSimplificationWarning = testSimplificationWarning
  makeObservationSimplificationWarning

testWarningSimple :: forall m. MonadThrow Error m => String -> String -> m Unit
testWarningSimple expression warning = testWarning (const [ warning ])
  (const expression)
  unit

testNoWarning :: forall m. MonadThrow Error m => String -> m Unit
testNoWarning expression = testWarning (const []) (const expression) unit

letSimplifies :: forall m. MonadThrow Error m => m Unit
letSimplifies =
  let
    simplifiableExpression =
      "(AddValue (SubValue (Constant 6) (NegValue (Constant -3))) (Constant -5))"

    simplification = "(Constant -2)"
  in
    testValueSimplificationWarning letContract simplifiableExpression
      simplification

depositSimplifies :: forall m. MonadThrow Error m => m Unit
depositSimplifies =
  let
    simplifiableExpression =
      "(AddValue (SubValue (Constant 3) (Constant -5)) (NegValue (Constant 7)))"

    simplification = "(Constant 1)"
  in
    testValueSimplificationWarning depositContract simplifiableExpression
      simplification

paySimplifies :: forall m. MonadThrow Error m => m Unit
paySimplifies =
  let
    simplifiableExpression =
      "(AddValue (SubValue (Constant 6) (Constant -1)) (NegValue (Constant 6)))"

    simplification = "(Constant 1)"
  in
    testValueSimplificationWarning payContractWithRole simplifiableExpression
      simplification

addValueSimplifies :: forall m. MonadThrow Error m => m Unit
addValueSimplifies =
  let
    simplifiableExpression =
      "(AddValue (NegValue (SubValue (Constant -1) (Constant 5))) (Constant -5))"

    simplification = "(Constant 1)"
  in
    testValueSimplificationWarning addContract simplifiableExpression
      simplification

addValueSimplifiesWithZero :: forall m. MonadThrow Error m => m Unit
addValueSimplifiesWithZero =
  let
    simplifiableExpression =
      "(AddValue TimeIntervalEnd (AddValue (NegValue (SubValue (Constant -1) (Constant 4))) (Constant -5)))"

    simplification = "TimeIntervalEnd"
  in
    testValueSimplificationWarning letContract simplifiableExpression
      simplification

subValueSimplifies :: forall m. MonadThrow Error m => m Unit
subValueSimplifies =
  let
    simplifiableExpression =
      "(SubValue (NegValue (SubValue (Constant -1) (Constant 5))) (Constant -2))"

    simplification = "(Constant 8)"
  in
    testValueSimplificationWarning subContract simplifiableExpression
      simplification

subValueSimplifiesWithZero :: forall m. MonadThrow Error m => m Unit
subValueSimplifiesWithZero =
  let
    simplifiableExpression =
      "(SubValue (AddValue (NegValue (SubValue (Constant -1) (Constant 4))) (Constant -5)) TimeIntervalEnd)"

    simplification = "(NegValue TimeIntervalEnd)"
  in
    testValueSimplificationWarning letContract simplifiableExpression
      simplification

notifySimplifies :: forall m. MonadThrow Error m => m Unit
notifySimplifies =
  let
    simplifiableExpression =
      "(OrObs (ValueLT TimeIntervalEnd (Constant 34)) (OrObs (NotObs (ValueEQ (AddValue (NegValue (Constant 2)) (Constant 5)) (Constant 3))) (NotObs (OrObs TrueObs FalseObs))))"

    simplification = "(ValueLT TimeIntervalEnd (Constant 34))"
  in
    testObservationSimplificationWarning notifyContract simplifiableExpression
      simplification

assertSimplifies :: forall m. MonadThrow Error m => m Unit
assertSimplifies =
  let
    simplifiableExpression =
      "(AndObs (ValueGT (Constant 14) TimeIntervalEnd) (AndObs (ValueEQ (AddValue (NegValue (Constant 2)) (Constant 5)) (Constant 3)) (OrObs FalseObs TrueObs)))"

    simplification = "(ValueGT (Constant 14) TimeIntervalEnd)"
  in
    testObservationSimplificationWarning assertContract simplifiableExpression
      simplification

ifSimplifies :: forall m. MonadThrow Error m => m Unit
ifSimplifies =
  let
    simplifiableExpression =
      "(OrObs (ValueGE TimeIntervalEnd (Constant 5)) (AndObs (NotObs (OrObs FalseObs (ValueEQ (AddValue (Constant -2) (Constant 3)) (Constant 1)))) TrueObs))"

    simplification = "(ValueGE TimeIntervalEnd (Constant 5))"
  in
    testObservationSimplificationWarning ifContract simplifiableExpression
      simplification

andObsSimplifies :: forall m. MonadThrow Error m => m Unit
andObsSimplifies =
  let
    simplifiableExpression =
      "(OrObs FalseObs (ValueEQ TimeIntervalEnd (Constant 2)))"

    simplification = "(ValueEQ TimeIntervalEnd (Constant 2))"
  in
    testObservationSimplificationWarning andContract simplifiableExpression
      simplification

andObsSimplifiesWithTrue :: forall m. MonadThrow Error m => m Unit
andObsSimplifiesWithTrue =
  let
    simplifiableExpression =
      "(AndObs TrueObs (ValueLE TimeIntervalEnd (Constant 6)))"

    simplification = "(ValueLE TimeIntervalEnd (Constant 6))"
  in
    testObservationSimplificationWarning ifContract simplifiableExpression
      simplification

orObsSimplifies :: forall m. MonadThrow Error m => m Unit
orObsSimplifies =
  let
    simplifiableExpression =
      "(AndObs TrueObs (ValueEQ TimeIntervalEnd (Constant 12)))"

    simplification = "(ValueEQ TimeIntervalEnd (Constant 12))"
  in
    testObservationSimplificationWarning orContract simplifiableExpression
      simplification

orObsSimplifiesWithFalse :: forall m. MonadThrow Error m => m Unit
orObsSimplifiesWithFalse =
  let
    simplifiableExpression =
      "(OrObs FalseObs (ValueGE TimeIntervalEnd (Constant 3)))"

    simplification = "(ValueGE TimeIntervalEnd (Constant 3))"
  in
    testObservationSimplificationWarning ifContract simplifiableExpression
      simplification

divZeroSimplified :: forall m. MonadThrow Error m => m Unit
divZeroSimplified =
  let
    simplifiableExpression = "(DivValue (Constant 0) (Constant 3))"

    simplification = "(Constant 0)"
  in
    testValueSimplificationWarning letContract simplifiableExpression
      simplification

divByZeroSimplified :: forall m. MonadThrow Error m => m Unit
divByZeroSimplified =
  let
    simplifiableExpression = "(DivValue (Constant 42) (Constant 0))"

    simplification = "(Constant 0)"
  in
    testValueSimplificationWarning letContract simplifiableExpression
      simplification

divConstantSimplified :: forall m. MonadThrow Error m => m Unit
divConstantSimplified =
  let
    simplifiableExpression = "(DivValue (Constant 7) (Constant -3))"

    simplification = "(Constant -2)"
  in
    testValueSimplificationWarning letContract simplifiableExpression
      simplification

letShadowing :: forall m. MonadThrow Error m => m Unit
letShadowing = testWarningSimple
  "Let \"value\" (Constant 1) (Let \"value\" (Constant 1) Close)"
  "Let is redefining a ValueId that already exists"

fromChars :: forall f. Foldable f => f Char -> String
fromChars = foldMap S.singleton

roleTooLongWarning :: forall m. MonadThrow Error m => m Unit
roleTooLongWarning = testWarningSimple contract
  "Role name is too long (role names are limited to 32 bytes)"
  where
  roleTooLong = fromChars $ replicate 33 'r'
  contract =
    "When [Case (Deposit (Role " <> show roleTooLong <>
      ") (Role \"alice\") (Token \"\" \"\") (Constant 10)) Close] 2 Close"

roleTooLongBytesWarning :: forall m. MonadThrow Error m => m Unit
roleTooLongBytesWarning = testWarningSimple contract
  "Role name is too long (role names are limited to 32 bytes)"
  where
  roleTooLong = S.singleton 'r' <> (fold $ replicate 16 "ü")
  contract =
    "When [Case (Deposit (Role " <> show roleTooLong <>
      ") (Role \"alice\") (Token \"\" \"\") (Constant 10)) Close] 2 Close"

roleOkNoWarning :: forall m. MonadThrow Error m => m Unit
roleOkNoWarning = testNoWarning contract
  where
  roleOk = fromChars $ replicate 32 'r'
  contract =
    "When [Case (Deposit (Role " <> show roleOk <>
      ") (Role \"alice\") (Token \"\" \"\") (Constant 10)) Close] 2 Close"

roleOkBytesNoWarning :: forall m. MonadThrow Error m => m Unit
roleOkBytesNoWarning = testNoWarning contract
  where
  roleOk = fold $ replicate 16 "ü"
  contract =
    "When [Case (Deposit (Role " <> show roleOk <>
      ") (Role \"alice\") (Token \"\" \"\") (Constant 10)) Close] 2 Close"

currencySymbolTooLongWarning :: forall m. MonadThrow Error m => m Unit
currencySymbolTooLongWarning = testWarningSimple contract
  "Policy ID is the wrong length (policy IDs must consist of 56 hexadecimal characters or 0 for ADA)"
  where
  currencySymbolTooLong = fromChars $ replicate 58 '0'
  contract =
    "When [Case (Deposit (Role \"alice\") (Role \"alice\") (Token "
      <> show currencySymbolTooLong
      <> " \"\") (Constant 10)) Close] 2 Close"

currencySymbolTooShortWarning :: forall m. MonadThrow Error m => m Unit
currencySymbolTooShortWarning = testWarningSimple contract
  "Policy ID is the wrong length (policy IDs must consist of 56 hexadecimal characters or 0 for ADA)"
  where
  currencySymbolTooLong = fromChars $ replicate 54 '0'
  contract =
    "When [Case (Deposit (Role \"alice\") (Role \"alice\") (Token "
      <> show currencySymbolTooLong
      <> " \"\") (Constant 10)) Close] 2 Close"

currencySymbolOkNoWarning :: forall m. MonadThrow Error m => m Unit
currencySymbolOkNoWarning = testNoWarning contract
  where
  currencySymbolOk = fromChars $ replicate 56 '0'
  contract =
    "When [Case (Deposit (Role \"alice\") (Role \"alice\") (Token "
      <> show currencySymbolOk
      <> " \"\") (Constant 10)) Close] 2 Close"

tokenNameTooLongWarning :: forall m. MonadThrow Error m => m Unit
tokenNameTooLongWarning = testWarningSimple contract
  "Token name is too long (token names are limited to 32 bytes)"
  where
  currencySymbolOk = fromChars $ replicate 56 '0'
  tokenNameTooLong = fromChars $ replicate 33 'r'
  contract =
    "When [Case (Deposit (Role \"alice\") (Role \"alice\") (Token "
      <> show currencySymbolOk
      <> " "
      <> show tokenNameTooLong
      <> ") (Constant 10)) Close] 2 Close"

tokenNameTooLongBytesWarning :: forall m. MonadThrow Error m => m Unit
tokenNameTooLongBytesWarning = testWarningSimple contract
  "Token name is too long (token names are limited to 32 bytes)"
  where
  currencySymbolOk = fromChars $ replicate 56 '0'
  tokenNameTooLong = S.singleton 'r' <> (fold $ replicate 16 "ü")
  contract =
    "When [Case (Deposit (Role \"alice\") (Role \"alice\") (Token "
      <> show currencySymbolOk
      <> " "
      <> show tokenNameTooLong
      <> ") (Constant 10)) Close] 2 Close"

tokenNameOkNoWarning :: forall m. MonadThrow Error m => m Unit
tokenNameOkNoWarning = testNoWarning contract
  where
  currencySymbolOk = fromChars $ replicate 56 '0'
  tokenNameOk = fromChars $ replicate 32 'r'
  contract =
    "When [Case (Deposit (Role \"alice\") (Role \"alice\") (Token "
      <> show currencySymbolOk
      <> " "
      <> show tokenNameOk
      <> ") (Constant 10)) Close] 2 Close"

tokenNameOkBytesNoWarning :: forall m. MonadThrow Error m => m Unit
tokenNameOkBytesNoWarning = testNoWarning contract
  where
  currencySymbolOk = fromChars $ replicate 56 '0'
  tokenNameOk = fold $ replicate 16 "ü"
  contract =
    "When [Case (Deposit (Role \"alice\") (Role \"alice\") (Token "
      <> show currencySymbolOk
      <> " "
      <> show tokenNameOk
      <> ") (Constant 10)) Close] 2 Close"

networkMismatchWarning :: forall m. MonadThrow Error m => m Unit
networkMismatchWarning = testWarningSimple contract
  "The contract uses addresses from both mainnet and testned. This is very dangerous and can make the Marlowe validator fail to run."
  where
  addressMainnet =
    "Address \"addr1qxn6u4ffhafpfvsw876wxllvvae88wekwhsnvpuh4s8fgf0xjsn7s0z25ycztthswazwj7wj0yta5m7d0y32q5aseyys63phd5\""
  addressTestnet =
    "Address \"addr_test1qzn6u4ffhafpfvsw876wxllvvae88wekwhsnvpuh4s8fgf0xjsn7s0z25ycztthswazwj7wj0yta5m7d0y32q5aseyyse8uhpt\""
  contract =
    "When [Case (Deposit ("
      <> addressMainnet
      <> ") ("
      <> addressTestnet
      <> ") (Token \"\" \"\") (Constant 10)) Close] 2 Close"

mainnetNetworkNoWarning :: forall m. MonadThrow Error m => m Unit
mainnetNetworkNoWarning = testNoWarning contract
  where
  addressMainnet =
    "Address \"addr1qxn6u4ffhafpfvsw876wxllvvae88wekwhsnvpuh4s8fgf0xjsn7s0z25ycztthswazwj7wj0yta5m7d0y32q5aseyys63phd5\""
  contract =
    "When [Case (Deposit ("
      <> addressMainnet
      <> ") ("
      <> addressMainnet
      <> ") (Token \"\" \"\") (Constant 10)) Close] 2 Close"

testnetNetworkNoWarning :: forall m. MonadThrow Error m => m Unit
testnetNetworkNoWarning = testNoWarning contract
  where
  addressTestnet =
    "Address \"addr_test1qzn6u4ffhafpfvsw876wxllvvae88wekwhsnvpuh4s8fgf0xjsn7s0z25ycztthswazwj7wj0yta5m7d0y32q5aseyyse8uhpt\""
  contract =
    "When [Case (Deposit ("
      <> addressTestnet
      <> ") ("
      <> addressTestnet
      <> ") (Token \"\" \"\") (Constant 10)) Close] 2 Close"

nonIncreasingTimeouts :: forall m. MonadThrow Error m => m Unit
nonIncreasingTimeouts = testWarningSimple "When [] 5 (When [] 5 Close)"
  "Timeouts should always increase in value"

unreachableThen :: forall m. MonadThrow Error m => m Unit
unreachableThen = testWarningSimple "If FalseObs Close Close" $ show
  UnreachableContract

unreachableElse :: forall m. MonadThrow Error m => m Unit
unreachableElse = testWarningSimple "If TrueObs Close Close" $ show
  UnreachableContract

unreachableCaseNotify :: forall m. MonadThrow Error m => m Unit
unreachableCaseNotify =
  testWarningSimple "When [Case (Notify FalseObs) Close] 10 Close"
    "This case will never be used, because the Observation is always false"

unreachableCaseEmptyChoiceList :: forall m. MonadThrow Error m => m Unit
unreachableCaseEmptyChoiceList =
  testWarningSimple
    "When [Case (Choice (ChoiceId \"choice\" (Role \"alice\")) []) Close] 10 Close"
    $ show UnreachableCaseEmptyChoice

unreachableCaseInvalidBound :: forall m. MonadThrow Error m => m Unit
unreachableCaseInvalidBound =
  testWarningSimple
    "When [Case (Choice (ChoiceId \"choice\" (Role \"alice\")) [Bound 0 2, Bound 4 3]) Close] 10 Close"
    $ show InvalidBound

undefinedLet :: forall m. MonadThrow Error m => m Unit
undefinedLet =
  testWarningSimple (letContract "(UseValue \"simplifiableValue\")") $ show
    UndefinedUse

undefinedChoiceValue :: forall m. MonadThrow Error m => m Unit
undefinedChoiceValue =
  testWarningSimple
    ( choiceAndThenDo
        ( addParenthesis
            ( payContractWithRole
                "(ChoiceValue (ChoiceId \"choice\" (Role \"role2\")))"
            )
        )
    ) $ show UndefinedChoice

nonPositiveDeposit :: forall m. MonadThrow Error m => m Unit
nonPositiveDeposit = testWarningSimple (depositContract "(Constant 0)") $ show
  NegativeDeposit

negativeDeposit :: forall m. MonadThrow Error m => m Unit
negativeDeposit = testWarningSimple (depositContract "(Constant -1)") $ show
  NegativeDeposit

nonPositivePay :: forall m. MonadThrow Error m => m Unit
nonPositivePay = testWarningSimple (payContractWithRole "(Constant 0)") $ show
  NegativePayment

negativePay :: forall m. MonadThrow Error m => m Unit
negativePay = testWarningSimple (payContractWithRole "(Constant -1)") $ show
  NegativePayment

payBeforeWarning :: forall m. MonadThrow Error m => m Unit
payBeforeWarning = testWarningSimple contract
  "The contract makes a payment from account \"role\" before a deposit has been made"
  where
  contract =
    "When [Case (Deposit (Role \"role1\" ) (Role \"role\") (Token \"\" \"\") (Constant 100)) (Pay (Role \"role\") (Party (Role \"role\")) (Token \"\" \"\") (Constant 1) Close)] 10 Close"

payBeforeWarningBranch :: forall m. MonadThrow Error m => m Unit
payBeforeWarningBranch = testWarningSimple contract
  "The contract makes a payment from account \"role\" before a deposit has been made"
  where
  contract =
    "When [Case (Deposit (Role \"role\") (Role \"role\") (Token \"\" \"\") (Constant 10)) Close] 2 (Pay (Role \"role\") (Party (Role \"role\")) (Token \"\" \"\") (Constant 10) Close)"

payDepositDifferentCurrency :: forall m. MonadThrow Error m => m Unit
payDepositDifferentCurrency = testWarningSimple
  (depositAndThenDo "(Constant 10)" continuation)
  "The contract makes a payment from account \"role\" before a deposit has been made"
  where
  continuation =
    "(Pay (Role \"role\") (Party (Role \"role\")) (Token \"0000\" \"0000\") (Constant 10) Close)"

payInsufficientDeposit :: forall m. MonadThrow Error m => m Unit
payInsufficientDeposit = testWarningSimple
  (depositAndThenDo "(Constant 9)" continuation)
  "The contract makes a payment of ₳ 0.000010 from account \"role\" but the account only has ₳ 0.000009"
  where
  continuation =
    "(Pay (Role \"role\") (Party (Role \"role\")) (Token \"\" \"\") (Constant 10) Close)"

payTwiceInsufficientDeposit :: forall m. MonadThrow Error m => m Unit
payTwiceInsufficientDeposit = testWarningSimple
  (depositAndThenDo "(Constant 9)" continuation)
  "The contract makes a payment of ₳ 0.000005 from account \"role\" but the account only has ₳ 0.000004"
  where
  continuation =
    "(Pay (Role \"role\") (Party (Role \"role\")) (Token \"\" \"\") (Constant 5) "
      <>
        "(Pay (Role \"role\") (Party (Role \"role\")) (Token \"\" \"\") (Constant 5) Close))"

payToHole :: forall m. MonadThrow Error m => m Unit
payToHole = testNoWarning contract
  where
  contract =
    "When [Case (Deposit (Role \"role\" ) (Role \"role\") (Token \"\" \"\") (Constant 100)) (Pay ?party (Party (Role \"role\")) (Token \"\" \"\") (Constant 1) Close)] 10 Close"

payThroughAccount :: forall m. MonadThrow Error m => m Unit
payThroughAccount = testNoWarning
  (depositAndThenDo "(Constant 10)" continuation)
  where
  continuation =
    "(Pay (Role \"role\") (Account (Role \"role2\")) (Token \"\" \"\") (Constant 10) "
      <>
        "(Pay (Role \"role2\") (Party (Role \"role\")) (Token \"\" \"\") (Constant 10) Close))"

payTwice :: forall m. MonadThrow Error m => m Unit
payTwice = testNoWarning (depositAndThenDo "(Constant 10)" continuation)
  where
  continuation =
    "(Pay (Role \"role\") (Party (Role \"role\")) (Token \"\" \"\") (Constant 5) "
      <>
        "(Pay (Role \"role\") (Party (Role \"role\")) (Token \"\" \"\") (Constant 5) Close))"

normalLet :: forall m. MonadThrow Error m => m Unit
normalLet = testNoWarning
  "Let \"a\" (Constant 0) (Let \"b\" (UseValue \"a\") Close)"

normalChoiceValue :: forall m. MonadThrow Error m => m Unit
normalChoiceValue = testNoWarning
  ( choiceAndThenDo
      ( addParenthesis
          ( payContractWithRole
              "(ChoiceValue (ChoiceId \"choice\" (Role \"role\")))"
          )
      )
  )

positiveDeposit :: forall m. MonadThrow Error m => m Unit
positiveDeposit = testNoWarning (depositContract "(Constant 1)")

positivePay :: forall m. MonadThrow Error m => m Unit
positivePay = testNoWarning (payContractWithRole "(Constant 1)")

paymentAddressPay :: forall m. MonadThrow Error m => m Unit
paymentAddressPay = testNoWarning
  (payContractWithPaymentKeyAddress "(Constant 1)")

scriptAddressPay :: forall m. MonadThrow Error m => m Unit
scriptAddressPay = testWarningSimple
  (payContractWithPaymentKeyAddress "(Constant 1)")
  (show invalidAddress <> " is not a valid Shelley payment key address")

