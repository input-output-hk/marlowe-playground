module Component.DecimalInput where

import Prelude

import Data.Decimal (Decimal)
import Data.Decimal as D
import Data.Either (Either(..))
import Data.Int as I
import Data.Lens (set) as L
import Data.Lens.Record (prop) as L
import Data.Maybe (Maybe(..))
import Data.Number (pow)
import Data.Numbers.Natural (Natural)
import Data.Numbers.Natural as N
import Effect.Class (class MonadEffect)
import Halogen (RefLabel(..))
import Halogen as H
import Halogen.Css (classNames) as HH
import Halogen.HTML (HTML, input) as HH
import Halogen.HTML.Events (onBlur, onValueInput) as HH
import Halogen.HTML.Properties
  ( InputType(..)
  , StepValue(..)
  , placeholder
  , ref
  , step
  , type_
  , value
  ) as HH
import Halogen.HTML.Properties.ARIA (label, role) as HH
import Type.Prelude (Proxy(..))

type Input =
  { classList :: Array String
  -- ^ Optional classes to style the component
  , precision :: Natural
  -- ^ Number of decimals
  , value :: Decimal
  -- ^ Initial value
  }

data Action
  = OnBlur
  | OnInput String
  | Receive Input

type Output = Either String Decimal

type State =
  { classList :: Array String
  , precision :: Natural
  , value :: String
  }

valueAsString :: Decimal -> Natural -> String
valueAsString value _ | value == zero = ""
valueAsString value precision = printDecimal precision value

component
  :: forall query m
   . MonadEffect m
  => H.Component query Input Output m
component = H.mkComponent
  { initialState: \i -> L.set (L.prop (Proxy :: Proxy "value"))
      (valueAsString i.value i.precision)
      i
  , render
  , eval: H.mkEval $
      H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
  }

handleAction
  :: forall m slots
   . MonadEffect m
  => Action
  -> H.HalogenM State Action slots Output m Unit
handleAction (OnInput s) = H.modify_ _ { value = s }

handleAction OnBlur = do
  state <- H.get
  case D.fromString state.value of
    Just v -> do
      H.raise $ Right v
    Nothing -> do
      H.raise $ Left state.value

handleAction (Receive input) = do
  state <- H.get
  when (state.value /= valueAsString input.value input.precision)
    $ H.modify_ _ { value = valueAsString input.value input.precision }

printDecimal :: Natural -> Decimal -> String
printDecimal precision = D.toFixed (N.toInt precision)

refLabel :: H.RefLabel
refLabel = RefLabel "Component.DecimalInput.input"

render :: forall w. State -> HH.HTML w Action
render state = HH.input
  [ HH.classNames $ state.classList
  , HH.onValueInput OnInput
  , HH.onBlur $ const $ OnBlur
  , HH.step $ HH.Step (pow 10.0 (I.toNumber $ -1 * N.toInt state.precision))
  , HH.placeholder $ printDecimal state.precision $ D.fromInt 0
  , HH.type_ HH.InputNumber
  , HH.value $ state.value
  , HH.ref refLabel
  , HH.role "spinbutton"
  , HH.label "Decimal Amount"
  ]

