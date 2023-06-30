module Component.Demos.View where

import Prologue hiding (div)

import Component.Demos.Types (Action(..), Demo)
import Component.Modal.ViewHelpers (modalHeader)
import Component.Projects.Types (Lang(..))
import Data.Newtype (wrap)
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..), ComponentHTML)
import Halogen.Classes (group, modalContent)
import Halogen.Css (classNames)
import Halogen.HTML (HTML, button, div, div_, h2_, hr_, span, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (class_, classes)
import Halogen.HTML.Properties.ARIA (label)
import MainFrame.Types (ChildSlots)

render :: forall m. MonadAff m => ComponentHTML Action ChildSlots m
render =
  div_
    [ modalHeader "Demo Files" (Just Cancel)
    , div [ classes [ modalContent, ClassName "projects-container" ] ]
        [ demoFile (wrap "Escrow") "Escrow"
            "Regulates a money exchange between a \"Buyer\" and a \"Seller\". If there is a disagreement, a \"Mediator\" will decide whether the money is refunded or paid to the \"Seller\"."
        , demoFile (wrap "EscrowWithCollateral") "Escrow With Collateral"
            "Regulates a money exchange between a \"Buyer\" and a \"Seller\" using a collateral from both parties to incentivize collaboration. If there is a disagreement the collateral is burned."
        , demoFile (wrap "ZeroCouponBond") "Zero Coupon Bond"
            "A simple loan. The investor pays the issuer the discounted price at the start, and is repaid the full (notional) price at the end."
        , demoFile (wrap "CouponBondGuaranteed") "Coupon Bond Guaranteed"
            "Debt agreement between an \"Lender\" and an \"Borrower\". \"Lender\" will advance the \"Principal\" amount at the beginning of the contract, and the \"Borrower\" will pay back \"Interest instalment\" every 30 slots and the \"Principal\" amount by the end of 3 instalments. The debt is backed by a collateral provided by the \"Guarantor\" which will be refunded as long as the \"Borrower\" pays back on time."
        , demoFile (wrap "Swap") "Swap"
            "Takes Ada from one party and dollar tokens from another party, and it swaps them atomically."
        , demoFile (wrap "CFD") "Contract For Differences"
            "\"Party\" and \"Counterparty\" deposit 100 Ada and after 60 slots is redistributed depending on the change in a given trade price reported by \"Oracle\". If the price increases, the difference goes to \"Counterparty\"; if it decreases, the difference goes to \"Party\", up to a maximum of 100 Ada."
        , demoFile (wrap "CFDWithOracle") "Contract For Differences with Oracle"
            "\"Party\" and \"Counterparty\" deposit 100 Ada and after 60 slots these assets are redistributed depending on the change in price of 100 Ada worth of dollars between the start and the end of the contract. If the price increases, the difference goes to \"Counterparty\"; if it decreases, the difference goes to \"Party\", up to a maximum of 100 Ada."
        ]
    ]

demoFile :: forall p. Demo -> String -> String -> HTML p Action
demoFile key name description =
  div []
    [ h2_ [ text name ]
    , div [ class_ group ]
        [ span [ class_ (ClassName "description") ] [ text description ]
        , div [ classes [ group, ClassName "open-buttons" ] ]
            [ button
                [ onClick $ const $ LoadDemo Haskell key
                , classNames [ "btn" ]
                , label $ name <> " Haskell"
                ]
                [ text "Haskell" ]
            , button
                [ onClick $ const $ LoadDemo Javascript key
                , classNames [ "btn" ]
                , label $ name <> " Javascript"
                ]
                [ text "Javascript" ]
            , button
                [ onClick $ const $ LoadDemo Marlowe key
                , classNames [ "btn" ]
                , label $ name <> " Marlowe"
                ]
                [ text "Marlowe" ]
            , button
                [ onClick $ const $ LoadDemo Blockly key
                , classNames [ "btn" ]
                , label $ name <> " Blockly"
                ]
                [ text "Blockly" ]
            ]
        ]
    , hr_
    ]
