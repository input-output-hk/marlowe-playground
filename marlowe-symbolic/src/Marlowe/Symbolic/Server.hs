{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Marlowe.Symbolic.Server where

import Control.Exception (evaluate)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as JSON
import Data.Bifunctor (first)
import Data.ByteString.Lazy.UTF8 as BSU
import Data.Proxy (Proxy (Proxy))
import Formatting (fprint, (%))
import Formatting.Clock (timeSpecs)
import Language.Marlowe (POSIXTime (..), TransactionInput, TransactionWarning)
import Language.Marlowe.Analysis.FSSemantics (warningsTraceCustom)
import Marlowe.Symbolic.Types.Request (Request (..))
import Marlowe.Symbolic.Types.Response (POSIXTimeWrapper (..), Response (..), Result (..))
import Servant (Application, JSON, Post, ReqBody, Server, serve, (:>))
import System.Clock (Clock (Monotonic), diffTimeSpec, getTime, toNanoSecs)

type API = "api" :> "marlowe-analysis" :> ReqBody '[JSON] Request :> Post '[JSON] Response

makeResult ::
  Either String (Maybe (POSIXTime, [TransactionInput], [TransactionWarning])) ->
  Result
makeResult (Left err) = Error (show err)
makeResult (Right res) =
  case res of
        Nothing -> Valid
        Just (POSIXTime iti, ti, tw) ->
          CounterExample
            { initialTime = POSIXTimeWrapper iti
            , transactionList = ti
            , transactionWarning = tw
            }

handlers :: Server API
handlers Request {..} =
  liftIO $ do
    start <- getTime Monotonic
    evRes <- warningsTraceCustom onlyAssertions contract (Just state)
    _ <- evaluate evRes
    end <- getTime Monotonic
    let res = Response { result = makeResult (first show evRes)
                       , durationMs = (toNanoSecs $ diffTimeSpec start end) `div` 1000000
                       }
    putStrLn $ BSU.toString $ JSON.encode res
    fprint ("Static analysis took " % timeSpecs % "\n") start end
    pure res

app :: Application
app = serve (Proxy @API) handlers

initializeContext :: IO ()
initializeContext = pure ()

initializeApplication :: IO Application
initializeApplication = pure app
