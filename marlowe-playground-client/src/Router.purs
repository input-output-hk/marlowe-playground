module Router where

import Prologue hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Newtype (unwrap)
import Data.Profunctor (dimap)
import Gists.Extra (GistId(..))
import Routing.Duplex (RouteDuplex', optional, param, record, root, (:=))
import Routing.Duplex as RD
import Routing.Duplex as Route
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/), (?))
import Type.Proxy (Proxy(..))

type Route =
  { subroute :: SubRoute
  , gistId :: Maybe GistId
  }

data MarloweView = MarloweBlocklyView | MarloweEditorView

derive instance Eq MarloweView

derive instance Generic MarloweView _

marloweViewToString :: MarloweView -> String
marloweViewToString = case _ of
  MarloweBlocklyView -> "blockly"
  MarloweEditorView -> "editor"

marloweViewFromString :: String -> Either String MarloweView
marloweViewFromString = case _ of
  "blockly" -> Right MarloweBlocklyView
  "editor" -> Right MarloweEditorView
  val -> Left $ "Not a Marlowe view: " <> val

marloweView :: RouteDuplex' String -> RouteDuplex' MarloweView
marloweView = RD.as marloweViewToString marloweViewFromString

data SubRoute
  = Home
  | Simulation
  | MarloweEditor
  | ImportContract
      { contract :: String
      , "marlowe-view" :: MarloweView
      }
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
          , "ImportContract": "importContract" ?
              { contract: RD.string, "marlowe-view": marloweView }
          , "HaskellEditor": "haskell" / noArgs
          , "JSEditor": "javascript" / noArgs
          , "Blockly": "blockly" / noArgs
          , "GithubAuthCallback": "gh-oauth-cb" / noArgs
          }
  where
  _importedContract = Proxy :: _ "importedContract"

  _gistId = Proxy :: _ "gistId"

  _subroute = Proxy :: _ "subroute"

printSubRoute :: SubRoute -> String
printSubRoute sr = Route.print route { gistId: Nothing, subroute: sr }
