{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators      #-}

module API where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Haskell.Interpreter (InterpreterError, InterpreterResult)
import Servant.API (Capture, Get, Header, Headers, JSON, NoContent, PlainText, Post, Raw, ReqBody, StdMethod (GET),
                    Verb, (:<|>), (:>))
import Web.Cookie (SetCookie)
import Webghc.Server (CompileRequest)

type API
     = "oracle" :> Capture "exchange" String :> Capture "pair" String :> Get '[JSON] Value
       :<|> "compile" :> ReqBody '[JSON] CompileRequest :> Post '[JSON] (Either InterpreterError (InterpreterResult String))
       :<|> "logout" :> Get '[JSON] (Headers '[Header "Set-Cookie" SetCookie] Value)
