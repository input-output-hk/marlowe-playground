module Component.Popper.Internal
  ( createPopper -- constructor
  -- instance methods
  , forceUpdate
  , destroyPopper
  -- Modifiers
  , arrow
  , popperOffsets
  , computeStyles
  , applyStyles
  , eventListeners
  , offset
  , preventOverflow
  , flipPlacement
  ) where

import Prelude

import Component.Popper.Types
  ( Boundary(..)
  , ComputeStyleOptions
  , EventListenerOptions
  , FlipOptions
  , Modifier
  , Offset
  , OffsetOption(..)
  , Options
  , Padding(..)
  , PaddingOption(..)
  , Placement(..)
  , PopperInstance
  , PositioningStrategy(..)
  , PreventOverflowOptions
  , Rect
  , RootBoundary(..)
  , TetherOffsetOption(..)
  )
import Data.Argonaut.Core (Json, fromNumber, fromString)
import Data.Argonaut.Encode (encodeJson)
import Data.Array as Array
import Data.Function.Uncurried (Fn1, Fn2, runFn1, runFn2)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Uncurried (EffectFn1, EffectFn3, runEffectFn1, runEffectFn3)
import Foreign.Object as Object
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (HTMLElement)

-------------------------------------------------------------------------------------------------------
-- General note on Modifiers:
-- PopperJS has a plugin architecture around modifiers. We offer FFI bindings to them using the
-- "plain" uncurried helpers.
-- There is no need to use the effect uncurried functions (death man last words) as we treat Modifiers
-- as opaque types. We don't compare them, and have no need for referential transparency with them.
-- Some of the modifiers don't receive any parameters (e.g. applyStyles), but they are still functions
-- of no argument. This helps the bundler to tree shake the modifiers we don't use.
----
foreign import _arrow :: Fn2 HTMLElement Json Modifier

arrow :: HTMLElement -> PaddingOption -> Modifier
arrow elm (PaddingValue padding) = runFn2 _arrow elm (paddingToFFI padding)

arrow elm (PaddingFn fn) = runFn2 _arrow elm $ unsafeCoerce
  (paddingToFFI <<< fn <<< optsFromFFI)

foreign import _popperOffsets :: Effect Modifier

popperOffsets :: Effect Modifier
popperOffsets = _popperOffsets

foreign import _computeStyles :: Fn1 ComputeStyleOptions Modifier

computeStyles :: ComputeStyleOptions -> Modifier
computeStyles = runFn1 _computeStyles

foreign import _applyStyles :: Effect Modifier

applyStyles :: Effect Modifier
applyStyles = _applyStyles

foreign import _eventListeners :: Fn1 EventListenerOptions Modifier

eventListeners :: EventListenerOptions -> Modifier
eventListeners = runFn1 _eventListeners

foreign import _offset :: Fn1 Json Modifier

offset :: OffsetOption -> Modifier
offset (OffsetValue value) = runFn1 _offset $ unsafeCoerce
  { offset: offsetToFFI value }

offset (OffsetFn fn) = runFn1 _offset $ unsafeCoerce
  { offset: (offsetToFFI <<< fn <<< optsFromFFI) }

foreign import _preventOverflow :: Fn1 Json Modifier

preventOverflow :: PreventOverflowOptions -> Modifier
preventOverflow
  { mainAxis
  , altAxis
  , padding
  , boundary
  , altBoundary
  , rootBoundary
  , tether
  , tetherOffset
  } =
  runFn1 _preventOverflow
    $ encodeJson
        { mainAxis
        , altAxis
        , padding: paddingToFFI padding
        , boundary: boundaryToFFI boundary
        , altBoundary
        , rootBoundary: rootBoundaryToFFI rootBoundary
        , tether
        , tetherOffset: tetherOffsetToFFI tetherOffset
        }

foreign import _flipPlacement :: Fn1 Json Modifier

flipPlacement :: FlipOptions -> Modifier
flipPlacement
  { padding
  , boundary
  , rootBoundary
  , flipVariations
  , allowedAutoPlacements
  } =
  runFn1 _flipPlacement
    $ encodeJson
        { padding: paddingToFFI padding
        , boundary: boundaryToFFI boundary
        , rootBoundary: rootBoundaryToFFI rootBoundary
        , flipVariations
        , allowedAutoPlacements: placementToFFI <$> allowedAutoPlacements
        }

--------------------------------------------------------------------------------
foreign import _createPopper
  :: EffectFn3 HTMLElement HTMLElement FFIOptions
       PopperInstance

createPopper :: HTMLElement -> HTMLElement -> Options -> Effect PopperInstance
createPopper reference popper options = runEffectFn3 _createPopper reference
  popper
  (optionsToFFI options)

foreign import _forceUpdate :: EffectFn1 PopperInstance Unit

forceUpdate :: PopperInstance -> Effect Unit
forceUpdate = runEffectFn1 _forceUpdate

foreign import _destroyPopper :: EffectFn1 PopperInstance Unit

destroyPopper :: PopperInstance -> Effect Unit
destroyPopper = runEffectFn1 _destroyPopper

--------------------------------------------------------------------------------
type FFIOptions =
  { placement :: String
  , modifiers :: Array Modifier
  , strategy :: String
  -- , onFirstUpdate ::
  }

optionsToFFI :: Options -> FFIOptions
optionsToFFI { placement, modifiers, strategy } =
  { placement: placementToFFI placement
  , modifiers
  , strategy: strategyToFFI strategy
  }

paddingToFFI :: Padding -> Json
paddingToFFI (Padding { top, left, right, bottom }) =
  encodeJson
    $ Object.fromFoldable
    $ Array.catMaybes
        [ Tuple "top" <$> top
        , Tuple "left" <$> left
        , Tuple "right" <$> right
        , Tuple "bottom" <$> bottom
        ]

boundaryToFFI :: Boundary -> Json
boundaryToFFI = case _ of
  ClippinParents -> fromString "clippingParents"
  SingleElementBoundary elm -> unsafeCoerce elm
  MultipleElementsBoundary elems -> unsafeCoerce elems

rootBoundaryToFFI :: RootBoundary -> String
rootBoundaryToFFI = case _ of
  DocumentBoundary -> "document"
  ViewportBoundary -> "viewport"

offsetToFFI :: Offset -> Json
offsetToFFI { skidding, distance } = encodeJson [ skidding, distance ]

tetherOffsetToFFI :: TetherOffsetOption -> Json
tetherOffsetToFFI = case _ of
  TetherOffset n -> fromNumber n
  TetherOffsetFn fn -> unsafeCoerce (fn <<< optsFromFFI)

placementToFFI :: Placement -> String
placementToFFI = case _ of
  Auto -> "auto"
  AutoStart -> "auto-start"
  AutoEnd -> "auto-end"
  Top -> "top"
  TopStart -> "top-start"
  TopEnd -> "top-end"
  Bottom -> "bottom"
  BottomStart -> "bottom-start"
  BottomEnd -> "bottom-end"
  Right -> "right"
  RightStart -> "right-start"
  RightEnd -> "right-end"
  Left -> "left"
  LeftStart -> "left-start"
  LeftEnd -> "left-end"

optsFromFFI
  :: { popper :: Rect, reference :: Rect, placement :: String }
  -> { popper :: Rect, reference :: Rect, placement :: Placement }
optsFromFFI { popper, reference, placement } =
  { popper, reference, placement: unsafePlacementFromFFI placement }

unsafePlacementFromFFI :: String -> Placement
unsafePlacementFromFFI = case _ of
  "auto" -> Auto
  "auto-start" -> AutoStart
  "auto-end" -> AutoEnd
  "top" -> Top
  "top-start" -> TopStart
  "top-end" -> TopEnd
  "bottom" -> Bottom
  "bottom-start" -> BottomStart
  "bottom-end" -> BottomEnd
  "right" -> Right
  "right-start" -> RightStart
  "right-end" -> RightEnd
  "left" -> Left
  "left-start" -> LeftStart
  "left-end" -> LeftEnd
  str -> unsafeThrow $ "Popper.js internal: Invalid placement string: " <> str

strategyToFFI :: PositioningStrategy -> String
strategyToFFI = case _ of
  Absolute -> "absolute"
  Fixed -> "fixed"
