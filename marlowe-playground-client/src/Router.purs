module Router where

import Prologue hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Newtype (unwrap)
import Data.Profunctor (dimap)
import Gists.Extra (GistId(..))
import Routing.Duplex (RouteDuplex', optional, param, record, root, (:=))
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Type.Proxy (Proxy(..))

type Route =
  { subroute :: SubRoute
  , gistId :: Maybe GistId
  }

data SubRoute
  = Home
  | Simulation
  | MarloweEditor
  | ImportContract String
  | HaskellEditor
  | JSEditor
  | Blockly
  | GithubAuthCallback

derive instance eqRoute :: Eq SubRoute

derive instance genericRoute :: Generic SubRoute _

route :: RouteDuplex' Route
route =
  root $ record
    # _gistId
        := optional (dimap unwrap GistId (param "gistid"))
    # _subroute
        := sum
          { "Home": noArgs
          , "Simulation": "simulation" / noArgs
          , "MarloweEditor": "marlowe" / noArgs
          , "ImportContract": "importContract" / (param "contract")
          , "HaskellEditor": "haskell" / noArgs
          , "JSEditor": "javascript" / noArgs
          , "Blockly": "blockly" / noArgs
          , "GithubAuthCallback": "gh-oauth-cb" / noArgs
          }
  where
  _importedContract = Proxy :: _ "importedContract"

  _gistId = Proxy :: _ "gistId"

  _subroute = Proxy :: _ "subroute"
