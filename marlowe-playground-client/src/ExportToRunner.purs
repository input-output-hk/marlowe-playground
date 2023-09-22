module ExportToRunner (exportToRunnerForm) where

import Prologue hiding (div)

import Component.Modal.ViewHelpers (modalHeader)
import Data.Tuple.Nested ((/\))
import Gists.View (idPublishGist)
import Halogen.Classes (modalContent)
import Halogen.HTML (ClassName(..), HTML, a, div, div_, p, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (classes)
import MainFrame.Types (Action(..), State)

exportToRunnerForm :: forall p. String -> State -> HTML p Action
exportToRunnerForm contractString _state =
  div_
    [ modalHeader "Export to Marlowe Runner" (Just CloseModal)
    , div [ classes [ modalContent ] ]
        [ p [ classes [ ClassName "mb-3" ] ]
            [ text
                "On what network would you like to deploy the contract?"
            ]
        , div_
            do
              (title /\ url) <-
                [ ("Mainnet" /\ "")
                , ("Preprod" /\ "")
                , ("Preview" /\ "")
                ]
              pure $ a
                [ idPublishGist
                , classes [ ClassName "auth-button", ClassName "mx-3" ]
                , onClick $ const $ SendToRunner url contractString
                ]
                [ text title
                ]
        ]
    ]
