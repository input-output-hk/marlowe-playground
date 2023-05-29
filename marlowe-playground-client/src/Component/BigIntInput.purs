module Component.BigIntInput where

import Prelude

import Data.BigInt.Argonaut (BigInt)
import Data.BigInt.Argonaut as BI
import Data.BigInt.Argonaut as BigInt
import Data.Either (Either(..))
import Data.Lens (set) as L
import Data.Lens.Record (prop) as L
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen (RefLabel(..))
import Halogen as H
import Halogen.Css (classNames) as HH
import Halogen.HTML (HTML, input) as HH
import Halogen.HTML.Events (onValueInput) as HH
import Halogen.HTML.Properties
  ( InputType(..)
  , StepValue(..)
  , placeholder
  , ref
  , step
  , type_
  , value
  ) as HH
import Type.Prelude (Proxy(..))

type Input =
  { classList :: Array String
  -- ^ Optional classes to style the component
  , value :: BigInt
  -- ^ Initial value
  }

data Action
  = ChangeValue String
  | Receive Input

type Output = Either String BigInt

type State =
  { classList :: Array String
  , value :: String
  }

valueAsString :: BigInt -> String
valueAsString value | value == zero = ""
valueAsString value = BigInt.toString value

component
  :: forall query m
   . MonadEffect m
  => H.Component query Input Output m
component = H.mkComponent
  { initialState: \i -> L.set (L.prop (Proxy :: Proxy "value"))
      (valueAsString i.value)
      i
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  }

handleAction
  :: forall m slots
   . MonadEffect m
  => Action
  -> H.HalogenM State Action slots Output m Unit
handleAction (Receive input) = do
  state <- H.get
  when (state.value /= BigInt.toString input.value)
    $ H.modify_ _ { value = valueAsString input.value }
handleAction (ChangeValue s) = do
  case BI.fromString s of
    Just v -> H.raise $ Right v
    Nothing -> H.raise $ Left s
  H.modify_ _ { value = s }

refLabel :: H.RefLabel
refLabel = RefLabel "Component.BigIntInput"

render :: forall w. State -> HH.HTML w Action
render state = HH.input
  [ HH.classNames $ state.classList
  , HH.onValueInput ChangeValue
  , HH.step $ HH.Step 1.0
  , HH.type_ HH.InputNumber
  , HH.value $ state.value
  , HH.placeholder "0"
  , HH.ref refLabel
  ]

