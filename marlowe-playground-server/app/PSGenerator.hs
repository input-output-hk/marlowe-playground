{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module PSGenerator
    ( generate
    ) where

import qualified API
import qualified Auth
import qualified ContractForDifferences
import qualified ContractForDifferencesWithOracle
import Control.Applicative (empty, (<|>))
import Control.Lens (set, (&))
import Control.Monad.Reader (MonadReader)
import qualified CouponBondGuaranteed
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid ()
import Data.Proxy (Proxy (Proxy))
import qualified Data.Set as Set ()
import qualified Data.Text.Encoding as T ()
import qualified Data.Text.IO as T ()
import qualified Escrow
import qualified EscrowWithCollateral
import qualified Example
import Gist (Gist, GistFile, NewGist, NewGistFile, Owner)
import Language.Haskell.Interpreter (CompilationError, InterpreterError, InterpreterResult, Warning)
import qualified Language.Marlowe.Core.V1.Semantics.Types as MC
import qualified Language.Marlowe.Core.V1.Semantics.Types.Address as MC.Address
import Language.Marlowe.Extended.V1
import Language.PureScript.Bridge (BridgePart, Language (Haskell, PureScript), PSType, SumType (..), TypeInfo (..),
                                   argonaut, buildBridge, equal, genericShow, mkSumType, order, psTypeParameters,
                                   typeModule, typeName, (^==))
import Language.PureScript.Bridge.Builder (BridgeData)
import Language.PureScript.Bridge.PSTypes (psString)
import Language.PureScript.Bridge.TypeParameters (A)
import Marlowe.Contracts (contractForDifferences, contractForDifferencesWithOracle, couponBondGuaranteed, escrow,
                          escrowWithCollateral, example, swap, zeroCouponBond)
import qualified Marlowe.Symbolic.Server as MS
import qualified Marlowe.Symbolic.Types.Request as MSReq
import qualified Marlowe.Symbolic.Types.Response as MSRes
import qualified Plutus.V2.Ledger.Api as P
import qualified PlutusTx.AssocMap as AssocMap
import qualified PlutusTx.AssocMap as Map
import Servant ((:<|>), (:>))
import Servant.PureScript (HasBridge, Settings, addTypes, apiModuleName, defaultBridge, defaultSettings,
                           generateWithSettings, languageBridge)
import qualified Swap
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import qualified Webghc.Server as Webghc
import qualified ZeroCouponBond

psJson :: PSType
psJson = TypeInfo "web-common" "Data.RawJson" "RawJson" []

aesonValueBridge :: BridgePart
aesonValueBridge = do
    typeName ^== "Value"
    typeModule ^== "Data.Aeson.Types.Internal"
    pure psJson

-- See Note [GistID hotfix]
psGistId :: TypeInfo 'PureScript
psGistId = TypeInfo "marlowe-playground-client" "Gists.Extra" "GistId" []

-- See Note [GistID hotfix]
gistIdBridge :: BridgePart
gistIdBridge = do
    typeName ^== "GistId"
    typeModule ^== "Gist"
    pure psGistId

psContract :: MonadReader BridgeData m => m PSType
psContract =
    TypeInfo "web-common-marlowe" "Language.Marlowe.Core.V1.Semantics.Types" "Contract" <$>
    psTypeParameters

contractBridge :: BridgePart
contractBridge = do
    typeName ^== "Contract"
    typeModule ^== "Language.Marlowe.Core.V1.Semantics.Types"
    psContract

psState :: MonadReader BridgeData m => m PSType
psState =
    TypeInfo "web-common-marlowe" "Language.Marlowe.Core.V1.Semantics.Types" "State" <$>
    psTypeParameters

stateBridge :: BridgePart
stateBridge = do
    typeName ^== "State"
    typeModule ^== "Language.Marlowe.Core.V1.Semantics.Types"
    psState

psObservation :: MonadReader BridgeData m => m PSType
psObservation =
    TypeInfo "web-common-marlowe" "Language.Marlowe.Core.V1.Semantics.Types" "Observation" <$>
    psTypeParameters

observationBridge :: BridgePart
observationBridge = do
    typeName ^== "Observation"
    typeModule ^== "Language.Marlowe.Core.V1.Semantics.Types"
    psObservation

psValue :: MonadReader BridgeData m => m PSType
psValue =
    TypeInfo "web-common-marlowe" "Language.Marlowe.Core.V1.Semantics.Types" "Value" <$>
    psTypeParameters

valueBridge :: BridgePart
valueBridge = do
    typeName ^== "Value"
    typeModule ^== "Language.Marlowe.Core.V1.Semantics.Types"
    psValue

psTransactionInput :: MonadReader BridgeData m => m PSType
psTransactionInput =
    TypeInfo "web-common-marlowe" "Language.Marlowe.Core.V1.Semantics.Types" "TransactionInput" <$>
    psTypeParameters

transactionInputBridge :: BridgePart
transactionInputBridge = do
    typeName ^== "TransactionInput"
    typeModule ^== "Language.Marlowe.Core.V1.Semantics"
    psTransactionInput

psTransactionWarning :: MonadReader BridgeData m => m PSType
psTransactionWarning =
    TypeInfo "web-common-marlowe" "Language.Marlowe.Core.V1.Semantics.Types" "TransactionWarning" <$>
    psTypeParameters

transactionWarningBridge :: BridgePart
transactionWarningBridge = do
    typeName ^== "TransactionWarning"
    typeModule ^== "Language.Marlowe.Core.V1.Semantics"
    psTransactionWarning

dayBridge :: BridgePart
dayBridge = typeName ^== "Day" >> return psString

timeBridge :: BridgePart
timeBridge = typeName ^== "LocalTime" >> return psString

psBigInteger :: PSType
psBigInteger = TypeInfo "web-common" "Data.BigInt.Argonaut" "BigInt" []

integerBridge :: BridgePart
integerBridge = do
    typeName ^== "Integer"
    pure psBigInteger


headersBridge :: BridgePart
headersBridge = do
    typeModule ^== "Servant.API.ResponseHeaders"
    typeName ^== "Headers"
    -- Headers should have two parameters, the list of headers and the return type.
    psTypeParameters >>= \case
        [_, returnType] -> pure returnType
        _               -> empty

headerBridge :: BridgePart
headerBridge = do
    typeModule ^== "Servant.API.Header"
    typeName ^== "Header'"
    empty

servantBridge :: BridgePart
servantBridge = headersBridge <|> headerBridge

myBridge :: BridgePart
myBridge =
    -- See Note [GistID hotfix]
    gistIdBridge <|>
    aesonValueBridge <|>
    servantBridge <|>
    integerBridge <|>
    dayBridge <|>
    timeBridge <|>
    contractBridge <|>
    stateBridge <|>
    observationBridge <|>
    valueBridge <|>
    transactionInputBridge <|>
    transactionWarningBridge <|>
    defaultBridge

data MyBridge

myBridgeProxy :: Proxy MyBridge
myBridgeProxy = Proxy

instance HasBridge MyBridge where
    languageBridge _ = buildBridge myBridge



myTypes :: [SumType 'Haskell]
myTypes =
       [
         equal . genericShow . argonaut $ mkSumType @Gist
       , equal . genericShow . argonaut $ mkSumType @GistFile
       , argonaut $ mkSumType @NewGist
       , argonaut $ mkSumType @NewGistFile
       , equal . genericShow . argonaut $ mkSumType @Owner
       , equal . genericShow . argonaut $ mkSumType @Auth.AuthStatus
       , order . equal . genericShow . argonaut $ mkSumType @Auth.AuthRole
       , argonaut $ mkSumType @CompilationError
       , argonaut $ mkSumType @InterpreterError
       , argonaut $ mkSumType @Warning
       , argonaut $ mkSumType @(InterpreterResult A)
       , genericShow . argonaut $ mkSumType @MSRes.Response
       , genericShow . argonaut $ mkSumType @MSRes.Result
       , argonaut $ mkSumType @MSReq.Request
       , argonaut $ mkSumType @Webghc.CompileRequest
       ]

mySettings :: Settings
mySettings = defaultSettings
    & set apiModuleName "Marlowe"
    & addTypes myTypes

multilineString :: BS.ByteString -> BS.ByteString -> BS.ByteString
multilineString name value =
    "\n\n" <> name <> " :: String\n" <> name <> " = \"\"\"" <> value <> "\"\"\""

psModule :: BS.ByteString -> BS.ByteString -> BS.ByteString
psModule name body = "module " <> name <> " where" <> body

writePangramJson :: FilePath -> IO ()
writePangramJson outputDir = do
     let
         aliceAddress = Address MC.Address.testnet (P.Address (P.PubKeyCredential "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2") Nothing)
         bobRole = MC.Role "Bob"
         const100 = MC.Constant 100
         choiceId = MC.ChoiceId "choice" aliceAddress
         valueExpr = MC.AddValue const100 (MC.SubValue const100 (MC.NegValue const100))
         token = MC.Token "aa" "name"
     let pangram =
             MC.Assert MC.TrueObs
                 (MC.When
                     [ MC.Case (MC.Deposit aliceAddress aliceAddress ada valueExpr)
                         ( MC.Let (MC.ValueId "x") valueExpr
                             (MC.Pay aliceAddress (MC.Party bobRole) ada (MC.Cond MC.TrueObs (MC.UseValue (MC.ValueId "x")) (MC.UseValue (MC.ValueId "y"))) MC.Close)
                         )
                     , MC.Case (MC.Choice choiceId [Bound 0 1, Bound 10 20])
                         ( MC.If (MC.ChoseSomething choiceId `MC.OrObs` (MC.ChoiceValue choiceId `MC.ValueEQ` const100))
                             (MC.Pay aliceAddress (MC.Account aliceAddress) token (MC.DivValue (MC.AvailableMoney aliceAddress token) const100) MC.Close)
                             MC.Close
                         )
                     , MC.Case (MC.Notify (MC.AndObs (MC.TimeIntervalStart `MC.ValueLT` MC.TimeIntervalEnd) MC.TrueObs)) MC.Close
                     ]
                     (P.POSIXTime 100)
                     MC.Close
                 )
         encodedPangram = Aeson.encode pangram
         state =
             MC.State
             { MC.accounts = AssocMap.singleton (aliceAddress, token) 12
             , MC.choices = Map.singleton choiceId 42
             , MC.boundValues = Map.fromList [ (ValueId "x", 1), (ValueId "y", 2) ]
             , MC.minTime = P.POSIXTime 123
             }
         encodedState = Aeson.encode state
     createDirectoryIfMissing True (outputDir </> "JSON")
     BSL.writeFile (outputDir </> "JSON" </> "contract.json") encodedPangram
     BSL.writeFile (outputDir </> "JSON" </> "state.json") encodedState

writeUsecases :: FilePath -> IO ()
writeUsecases outputDir = do
    let haskellUsecases =
            multilineString "example" example
         <> multilineString "escrow" escrow
         <> multilineString "escrowWithCollateral" escrowWithCollateral
         <> multilineString "zeroCouponBond" zeroCouponBond
         <> multilineString "couponBondGuaranteed" couponBondGuaranteed
         <> multilineString "swap" swap
         <> multilineString "contractForDifferences" contractForDifferences
         <> multilineString "contractForDifferencesWithOracle" contractForDifferencesWithOracle
        haskellUsecasesModule = psModule "Examples.Haskell.Contracts" haskellUsecases
    createDirectoryIfMissing True (outputDir </> "Examples" </> "Haskell")
    BS.writeFile (outputDir </> "Examples" </> "Haskell" </> "Contracts.purs") haskellUsecasesModule
    let contractToString = BS8.pack . show . pretty
        marloweUsecases =
            multilineString "example" (contractToString Example.example)
         <> multilineString "escrow" (contractToString Escrow.escrow)
         <> multilineString "escrowWithCollateral" (contractToString EscrowWithCollateral.escrowC)
         <> multilineString "zeroCouponBond" (contractToString ZeroCouponBond.zcb)
         <> multilineString "couponBondGuaranteed" (contractToString CouponBondGuaranteed.cbg)
         <> multilineString "swap" (contractToString Swap.swap)
         <> multilineString "contractForDifferences" (contractToString ContractForDifferences.cfd)
         <> multilineString "contractForDifferencesWithOracle" (contractToString ContractForDifferencesWithOracle.cfd)
        marloweUsecasesModule = psModule "Examples.Marlowe.Contracts" marloweUsecases
    createDirectoryIfMissing True (outputDir </> "Examples" </> "Marlowe")
    BS.writeFile (outputDir </> "Examples" </> "Marlowe" </> "Contracts.purs") marloweUsecasesModule
    putStrLn outputDir

type Web = ("api" :> (API.API :<|> Auth.FrontendAPI)) :<|> MS.API :<|> Webghc.FrontendAPI

generate :: FilePath -> IO ()
generate outputDir = do
    generateWithSettings mySettings outputDir myBridgeProxy (Proxy @Web)
    writeUsecases outputDir
    writePangramJson outputDir
