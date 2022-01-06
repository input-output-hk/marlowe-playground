{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Language.Marlowe.Scripts where
import Data.Default (Default (def))
import Language.Marlowe.Semantics
import Language.Marlowe.SemanticsTypes hiding (Contract)
import Ledger
import Ledger.Ada (adaSymbol)
import Ledger.Constraints
import Ledger.Constraints.OnChain
import Ledger.Constraints.TxConstraints
import qualified Ledger.Interval as Interval
import qualified Ledger.TimeSlot as TimeSlot
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Value as Val
import Plutus.Contract.StateMachine (StateMachine (..), Void)
import qualified Plutus.Contract.StateMachine as SM
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AssocMap
import PlutusTx.Prelude
import Unsafe.Coerce

type MarloweSlotRange = (Slot, Slot)
type MarloweInput = (MarloweSlotRange, [Input])

-- Yeah, I know
type SmallUntypedTypedValidator = Scripts.TypedValidator Scripts.Any
type SmallTypedValidator = Scripts.TypedValidator TypedMarloweValidator

data TypedMarloweValidator

{- Type instances for small typed Marlowe validator -}
instance Scripts.ValidatorTypes (TypedMarloweValidator) where
    type instance RedeemerType (TypedMarloweValidator) = [Input]
    type instance DatumType (TypedMarloweValidator) = MarloweData


rolePayoutScript :: CurrencySymbol -> Validator
rolePayoutScript symbol = mkValidatorScript ($$(PlutusTx.compile [|| wrapped ||]) `PlutusTx.applyCode` PlutusTx.liftCode symbol)
  where
    wrapped s = Scripts.wrapValidator (rolePayoutValidator s)


{-# INLINABLE rolePayoutValidator #-}
rolePayoutValidator :: CurrencySymbol -> TokenName -> () -> ScriptContext -> Bool
rolePayoutValidator currency role _ ctx =
    Val.valueOf (valueSpent (scriptContextTxInfo ctx)) currency role > 0


mkRolePayoutValidatorHash :: CurrencySymbol -> ValidatorHash
mkRolePayoutValidatorHash symbol = validatorHash (rolePayoutScript symbol)


defaultRolePayoutValidatorHash :: ValidatorHash
defaultRolePayoutValidatorHash = mkRolePayoutValidatorHash adaSymbol

{-# INLINABLE mkMarloweStateMachineTransition #-}
mkMarloweStateMachineTransition
    :: MarloweParams
    -> SM.State MarloweData
    -> MarloweInput
    -> Maybe (TxConstraints Void Void, SM.State MarloweData)
mkMarloweStateMachineTransition params SM.State{ SM.stateData=MarloweData{..}, SM.stateValue=scriptInValue}
    (interval@(minSlot, maxSlot), inputs) = do
    let positiveBalances = validateBalances marloweState ||
            -- Avoid creating a too-big string literal
            traceError ("M1")

    {-  We do not check that a transaction contains exact input payments.
        We only require an evidence from a party, e.g. a signature for PubKey party,
        or a spend of a 'party role' token.
        This gives huge flexibility by allowing parties to provide multiple
        inputs (either other contracts or P2PKH).
        Then, we check scriptOutput to be correct.
     -}
    let inputsConstraints = validateInputs params inputs

    -- total balance of all accounts in State
    -- accounts must be positive, and we checked it above
    let inputBalance = totalBalance (accounts marloweState)

    -- ensure that a contract TxOut has what it suppose to have
    let balancesOk = inputBalance == scriptInValue

    let preconditionsOk = traceIfFalse "M2" $ positiveBalances && balancesOk

    let txInput = TransactionInput {
            txInterval = interval,
            txInputs = inputs }

    let computedResult = computeTransaction txInput marloweState marloweContract
    case computedResult of
        TransactionOutput {txOutPayments, txOutState, txOutContract} -> do

            let marloweData = MarloweData {
                    marloweContract = txOutContract,
                    marloweState = txOutState }

            let (outputsConstraints, finalBalance) = let
                    payoutsByParty = AssocMap.toList $ foldMap payoutByParty txOutPayments
                    in case txOutContract of
                        Close -> (payoutConstraints payoutsByParty, zero)
                        _ -> let
                            outputsConstraints = payoutConstraints payoutsByParty
                            totalIncome = foldMap (collectDeposits . getInputContent) inputs
                            totalPayouts = foldMap snd payoutsByParty
                            finalBalance = inputBalance + totalIncome - totalPayouts
                            in (outputsConstraints, finalBalance)
            -- TODO Push this use of time further down the code
            let range = TimeSlot.slotRangeToPOSIXTimeRange def $ Interval.interval minSlot maxSlot
            let constraints = inputsConstraints <> outputsConstraints <> mustValidateIn range
            if preconditionsOk
            then Just (constraints, SM.State marloweData finalBalance)
            else Nothing
        Error _ -> Nothing

  where
    validateInputs :: MarloweParams -> [Input] -> TxConstraints Void Void
    validateInputs MarloweParams{rolesCurrency} inputs = let
        (keys, roles) = foldMap (validateInputWitness . getInputContent) inputs
        mustSpendSetOfRoleTokens = foldMap mustSpendRoleToken (AssocMap.keys roles)
        in foldMap mustBeSignedBy keys <> mustSpendSetOfRoleTokens
      where
        validateInputWitness :: InputContent -> ([PubKeyHash], AssocMap.Map TokenName ())
        validateInputWitness input =
            case input of
                IDeposit _ party _ _         -> validatePartyWitness party
                IChoice (ChoiceId _ party) _ -> validatePartyWitness party
                INotify                      -> (mempty, mempty)
          where
            validatePartyWitness (PK pk)     = ([pk], mempty)
            validatePartyWitness (Role role) = ([], AssocMap.singleton role ())

        mustSpendRoleToken :: TokenName -> TxConstraints Void Void
        mustSpendRoleToken role = mustSpendAtLeast $ Val.singleton rolesCurrency role 1

    collectDeposits :: InputContent -> Val.Value
    collectDeposits (IDeposit _ _ (Token cur tok) amount) = Val.singleton cur tok amount
    collectDeposits _                                     = zero

    payoutByParty :: Payment -> AssocMap.Map Party Val.Value
    payoutByParty (Payment _ (Party party) money) = AssocMap.singleton party money
    payoutByParty (Payment _ (Account _) _)       = AssocMap.empty

    payoutConstraints :: [(Party, Val.Value)] -> TxConstraints i0 o0
    payoutConstraints payoutsByParty = foldMap payoutToTxOut payoutsByParty
      where
        payoutToTxOut (party, value) = case party of
            PK pk  -> mustPayToPubKey pk value
            Role role -> let
                dataValue = Datum $ PlutusTx.toBuiltinData role
                in mustPayToOtherScript (rolePayoutValidatorHash params) dataValue value


{-# INLINABLE isFinal #-}
isFinal :: MarloweData -> Bool
isFinal MarloweData{marloweContract=c} = isClose c

{-# INLINABLE mkValidator #-}
mkValidator :: MarloweParams -> Scripts.ValidatorType MarloweStateMachine
mkValidator p = SM.mkValidator $ SM.mkStateMachine Nothing (mkMarloweStateMachineTransition p) isFinal


{-# INLINABLE smallMarloweValidator #-}
smallMarloweValidator
    :: MarloweParams
    -> MarloweData
    -> [Input]
    -> ScriptContext
    -> Bool
smallMarloweValidator MarloweParams{rolesCurrency, rolePayoutValidatorHash} MarloweData{..} inputs ctx@ScriptContext{scriptContextTxInfo} = do
    let slotConfig = def :: TimeSlot.SlotConfig
    let ownInput = case findOwnInput ctx of
            Just i -> i
            _      -> traceError "I0" {-"Can't find validation input"-}
    let scriptInValue = txOutValue $ txInInfoResolved ownInput
    let (minTime, maxTime) =
            case txInfoValidRange scriptContextTxInfo of
                Interval.Interval (Interval.LowerBound (Interval.Finite l) True) (Interval.UpperBound (Interval.Finite h) False) -> (l, h)
                -- FIXME remove this when mockchain implementation updates to correct one as above
                Interval.Interval (Interval.LowerBound (Interval.Finite l) True) (Interval.UpperBound (Interval.Finite h) True) -> (l, h)
                _ -> traceError "R0"
    let timeToSlot = TimeSlot.posixTimeToEnclosingSlot slotConfig
    let minSlot = timeToSlot minTime
    let maxSlot = timeToSlot maxTime
    let interval = (minSlot, maxSlot)
    let positiveBalances = traceIfFalse "B0" $ validateBalances marloweState

    {-  We do not check that a transaction contains exact input payments.
        We only require an evidence from a party, e.g. a signature for PubKey party,
        or a spend of a 'party role' token.
        This gives huge flexibility by allowing parties to provide multiple
        inputs (either other contracts or P2PKH).
        Then, we check scriptOutput to be correct.
     -}
    let inputsOk = validateInputs inputs

    -- total balance of all accounts in State
    -- accounts must be positive, and we checked it above
    let inputBalance = totalBalance (accounts marloweState)

    -- ensure that a contract TxOut has what it suppose to have
    let balancesOk = traceIfFalse "B1" $ inputBalance == scriptInValue

    let preconditionsOk = positiveBalances && balancesOk

    let txInput = TransactionInput {
            txInterval = interval,
            txInputs = inputs }

    let computedResult = computeTransaction txInput marloweState marloweContract
    -- let computedResult = TransactionOutput [] [] (emptyState minSlot) Close
    case computedResult of
        TransactionOutput {txOutPayments, txOutState, txOutContract} -> do
            let marloweData = MarloweData {
                    marloweContract = txOutContract,
                    marloweState = txOutState }

                payoutsByParty = AssocMap.toList $ foldMap payoutByParty txOutPayments
                payoutsOk = payoutConstraints payoutsByParty
                checkContinuation = case txOutContract of
                    Close -> True
                    _ -> let
                        totalIncome = foldMap (collectDeposits . getInputContent) inputs
                        totalPayouts = foldMap snd payoutsByParty
                        finalBalance = inputBalance + totalIncome - totalPayouts
                        outConstrs = OutputConstraint
                                    { ocDatum = marloweData
                                    , ocValue = finalBalance
                                    }
                        in checkOwnOutputConstraint ctx outConstrs
            preconditionsOk && inputsOk && payoutsOk && checkContinuation
        Error TEAmbiguousSlotIntervalError -> traceError "E1"
        Error TEApplyNoMatchError -> traceError "E2"
        Error (TEIntervalError (InvalidInterval _)) -> traceError "E3"
        Error (TEIntervalError (IntervalInPastError _ _)) -> traceError "E4"
        Error TEUselessTransaction -> traceError "E5"
        Error TEHashMismatch -> traceError "E6"

  where
    checkScriptOutput addr hsh value TxOut{txOutAddress, txOutValue, txOutDatumHash=Just svh} =
                    txOutValue == value && hsh == Just svh && txOutAddress == addr
    checkScriptOutput _ _ _ _ = False

    allOutputs :: [TxOut]
    allOutputs = txInfoOutputs scriptContextTxInfo

    validateInputs :: [Input] -> Bool
    validateInputs inputs = all (validateInputWitness . getInputContent) inputs
      where
        validateInputWitness :: InputContent -> Bool
        validateInputWitness input =
            case input of
                IDeposit _ party _ _         -> validatePartyWitness party
                IChoice (ChoiceId _ party) _ -> validatePartyWitness party
                INotify                      -> True
          where
            validatePartyWitness (PK pk)     = traceIfFalse "S" $ scriptContextTxInfo `txSignedBy` pk
            validatePartyWitness (Role role) = traceIfFalse "T" -- "Spent value not OK"
                                               $ Val.singleton rolesCurrency role 1 `Val.leq` valueSpent scriptContextTxInfo

    collectDeposits :: InputContent -> Val.Value
    collectDeposits (IDeposit _ _ (Token cur tok) amount) = Val.singleton cur tok amount
    collectDeposits _                                     = zero

    payoutByParty :: Payment -> AssocMap.Map Party Val.Value
    payoutByParty (Payment _ (Party party) money) = AssocMap.singleton party money
    payoutByParty (Payment _ (Account _) _)       = AssocMap.empty

    payoutConstraints :: [(Party, Val.Value)] -> Bool
    payoutConstraints payoutsByParty = all payoutToTxOut payoutsByParty
      where
        payoutToTxOut (party, value) = case party of
            PK pk  -> traceIfFalse "P" $ value `Val.leq` valuePaidTo scriptContextTxInfo pk
            Role role -> let
                dataValue = Datum $ PlutusTx.toBuiltinData role
                hsh = findDatumHash dataValue scriptContextTxInfo
                addr = Ledger.scriptHashAddress rolePayoutValidatorHash
                in traceIfFalse "R" $ any (checkScriptOutput addr hsh value) allOutputs



mkMarloweValidatorCode
    :: MarloweParams
    -> PlutusTx.CompiledCode (Scripts.ValidatorType MarloweStateMachine)
mkMarloweValidatorCode params =
    $$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode params


type MarloweStateMachine = StateMachine MarloweData MarloweInput

typedValidator :: MarloweParams -> Scripts.TypedValidator MarloweStateMachine
typedValidator params = Scripts.mkTypedValidator @MarloweStateMachine
    (mkMarloweValidatorCode params)
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @MarloweData @MarloweInput


smallTypedValidator :: MarloweParams -> Scripts.TypedValidator TypedMarloweValidator
smallTypedValidator params = Scripts.mkTypedValidatorParam @TypedMarloweValidator
    $$(PlutusTx.compile [|| smallMarloweValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    params
    where
        wrap = Scripts.wrapValidator


smallUntypedValidator :: MarloweParams -> Scripts.TypedValidator TypedMarloweValidator
smallUntypedValidator params = let
    wrapped s = Scripts.wrapValidator (smallMarloweValidator s)
    typed = mkValidatorScript ($$(PlutusTx.compile [|| wrapped ||]) `PlutusTx.applyCode` PlutusTx.liftCode params)
    -- Yeah, I know. It works, though.
    -- Remove this when Typed Validator has the same size as untyped.
    in unsafeCoerce (Scripts.unsafeMkTypedValidator typed)


mkMachineInstance :: MarloweParams -> SM.StateMachineInstance MarloweData MarloweInput
mkMachineInstance params =
    SM.StateMachineInstance
    (SM.mkStateMachine Nothing (mkMarloweStateMachineTransition params) isFinal)
    (typedValidator params)


mkMarloweClient :: MarloweParams -> SM.StateMachineClient MarloweData MarloweInput
mkMarloweClient params = SM.mkStateMachineClient (mkMachineInstance params)


defaultTxValidationRange :: Slot
defaultTxValidationRange = 10