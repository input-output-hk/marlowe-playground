{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
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
import Control.Applicative ((<|>))
import Control.Lens (set, (&))
import Control.Monad.Reader (MonadReader)
import qualified CouponBondGuaranteed
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Monoid ()
import Data.Proxy (Proxy (Proxy))
import qualified Data.Set as Set ()
import qualified Data.Text.Encoding as T ()
import qualified Data.Text.IO as T ()
import qualified Escrow
import qualified EscrowWithCollateral
import qualified Example
import Language.Haskell.Interpreter (CompilationError, InterpreterError, InterpreterResult, SourceCode, Warning)
import Language.Marlowe.Extended.V1
import Language.PureScript.Bridge (BridgePart, Language (Haskell, PureScript), PSType, SumType (..), TypeInfo (..),
                                   argonaut, buildBridge, genericShow, mkSumType, psTypeParameters, typeModule,
                                   typeName, (^==))
import Language.PureScript.Bridge.Builder (BridgeData)
import Language.PureScript.Bridge.PSTypes (psString)
import Language.PureScript.Bridge.TypeParameters (A)
import Marlowe.Contracts (contractForDifferences, contractForDifferencesWithOracle, couponBondGuaranteed, escrow,
                          escrowWithCollateral, example, swap, zeroCouponBond)
import qualified Marlowe.Symbolic.Server as MS
import qualified Marlowe.Symbolic.Types.Request as MSReq
import qualified Marlowe.Symbolic.Types.Response as MSRes
-- import qualified PSGenerator.Common
import Servant ((:<|>), (:>))
import Servant.PureScript (HasBridge, Settings, addTypes, apiModuleName, defaultBridge, defaultSettings,
                           generateWithSettings, languageBridge)
import qualified Swap
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import qualified Webghc.Server as Webghc
import qualified ZeroCouponBond

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

myBridge :: BridgePart
myBridge =
    -- See Note [GistID hotfix]
    gistIdBridge <|>
    -- FIXME: Just commenting these, not removing until I make SCP-4726 to see that it
    --        does not affect the entire build
    -- PSGenerator.Common.aesonBridge <|>
    -- PSGenerator.Common.containersBridge <|>
    -- PSGenerator.Common.languageBridge <|>
    -- PSGenerator.Common.ledgerBridge <|>
    -- PSGenerator.Common.servantBridge <|>
    -- PSGenerator.Common.miscBridge <|>
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

-- -- See Note [GistID hotfix]
-- playgroundTypes' :: [SumType 'Haskell]
-- playgroundTypes' = filter (\(SumType TypeInfo{_typeName} _ _) -> _typeName /= "GistId" )
--     PSGenerator.Common.playgroundTypes


myTypes :: [SumType 'Haskell]
myTypes =
    -- PSGenerator.Common.ledgerTypes <>
    -- PSGenerator.Common.walletTypes <>
    -- playgroundTypes' <>
    [ argonaut $ mkSumType @SourceCode
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
