module Component.CurrencyInput where

import Prologue

import Component.BigIntInput as BII
import Component.BigIntInput as BigIntInput
import Component.DecimalInput as DI
import Component.DecimalInput as DecimalInput
import Contrib.Data.Decimal (fromBigInt, toBigInt) as D
import Data.BigInt.Argonaut (BigInt)
import Data.Decimal (Decimal)
import Data.Decimal (fromInt, pow) as D
import Data.Either (either)
import Data.Foldable (foldMap)
import Data.Maybe (isJust)
import Data.Monoid (guard)
import Data.Numbers.Natural (Natural)
import Data.Numbers.Natural as N
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Css (classNames)
import Halogen.HTML as HH
import Type.Prelude (Proxy(..))

type Output = BigInt

data Action
  = AmountParseError String
  | ChangeValue BigInt
  | Receive Input

type DataRecord r =
  { amountInMinor :: BigInt
  -- ^ Initial value expressed in minor currency
  , classList :: Array String
  -- ^ Optional classes to style the component
  , currencySymbol :: Maybe String
  -- ^ Symbol that represents the major or just currency
  | r
  }

type Input = DataRecord
  ( majorCurrencyFactor :: Maybe Natural
  -- ^ A factor value between minor and major currency
  )

type State = DataRecord
  ( amountParseError :: Maybe String
  , majorCurrencyRatio ::
      Maybe
        { precision :: Natural
        -- ^ Precision used by internal decimal field to format plain String value
        , ratio :: Decimal
        -- ^ Precomputed ratio between minor and major currency
        }
  )

type ChildSlots =
  ( decimalInput :: forall q. H.Slot q DI.Output Unit
  , bigIntInput :: forall q. H.Slot q BII.Output Unit
  )

type ComponentHTML m =
  H.ComponentHTML Action ChildSlots m

type Component query m = H.Component query Input Output m

type DSL m a =
  H.HalogenM State Action ChildSlots Output m a

data Query (a :: Type)

type Slot m = H.Slot Query Output m

component
  :: forall query m
   . MonadEffect m
  => Component query m
component = H.mkComponent
  { initialState: deriveState
  , render
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction, receive = Just <<< Receive }
  }

deriveState :: Input -> State
deriveState input = do
  let
    toRatio = map \precision ->
      { ratio: D.fromInt 10 `D.pow` D.fromInt (-1 * N.toInt precision)
      , precision
      }
  { amountInMinor: input.amountInMinor
  , classList: input.classList
  , currencySymbol: input.currencySymbol
  , amountParseError: Nothing
  , majorCurrencyRatio: toRatio input.majorCurrencyFactor
  }

handleAction
  :: forall m
   . MonadEffect m
  => Action
  -> DSL m Unit
handleAction (Receive input) = do
  state <- H.get
  let
    state' = deriveState input
  when (state /= state') $
    H.put state'

handleAction (ChangeValue value) = do
  H.modify_ _ { amountInMinor = value, amountParseError = Nothing }
  H.raise value

handleAction (AmountParseError err) = do
  H.modify_ _ { amountParseError = Just err }
  pure unit

render
  :: forall m
   . MonadEffect m
  => State
  -> ComponentHTML m
render state = do
  let
    hasErrors = isJust state.amountParseError
  HH.div
    [ classNames
        ( [ "bg-gray-light"
          , "flex"
          , "items-center"
          , "border-solid"
          , "border"
          , "rounded-sm"
          , "overflow-hidden"
          , "box-border"
          , "focus-within:ring-1"
          , "focus-within:ring-black"
          ]
            <> guard
              hasErrors
              [ "text-[color:red]" ]
            <> state.classList
        )
    ]
    $
      ( flip foldMap state.currencySymbol \symbol -> pure $
          HH.div
            [ classNames $
                [ "flex-none"
                , "px-2"
                , "py-0"
                , "box-border"
                , "self-center"
                ]
            ]
            [ HH.text symbol ]
      )
        <> [ renderInput state ]

renderInput
  :: forall m
   . MonadEffect m
  => State
  -> ComponentHTML m
renderInput state = case state.majorCurrencyRatio of
  Nothing -> do
    HH.slot (Proxy :: Proxy "bigIntInput") unit BigIntInput.component
      { classList
      , value: state.amountInMinor
      }
      (either AmountParseError ChangeValue)
  Just { precision, ratio } -> do
    let
      amountInMajor = D.fromBigInt state.amountInMinor * ratio

      toMinor :: Decimal -> BigInt
      toMinor a = D.toBigInt $ a / ratio

    HH.slot (Proxy :: Proxy "decimalInput") unit DecimalInput.component
      { classList
      , precision
      , value: amountInMajor
      }
      (either AmountParseError (ChangeValue <<< toMinor))
  where
  classList =
    [ "flex-1"
    , "px-1"
    , "box-border"
    , "self-stretch"
    , "border-0"
    , "outline-none"
    ]

