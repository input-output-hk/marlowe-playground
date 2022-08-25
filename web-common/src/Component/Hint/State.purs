module Component.Hint.State (component, hint) where

import Prologue

import Component.Hint.Lenses
  ( _active
  , _content
  , _mGlobalClickSubscription
  , _mPopperInstance
  , _placement
  )
import Component.Hint.Types
  ( Action(..)
  , Input
  , State
  , arrowRef
  , hintRef
  , popoutRef
  )
import Component.Hint.View (render)
import Component.Popper
  ( OffsetOption(..)
  , PaddingOption(..)
  , Placement
  , PositioningStrategy(..)
  , arrow
  , createPopper
  , defaultFlip
  , defaultModifiers
  , defaultPreventOverflow
  , destroyPopper
  , flipPlacement
  , forceUpdate
  , offset
  , pAll
  , preventOverflow
  )
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.MonadPlus (guard)
import Data.Filterable (filterMap)
import Data.Foldable (and, for_)
import Data.Int (toNumber)
import Data.Lens (assign, set, use)
import Data.Traversable (for, traverse)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Unsafe (unsafePerformEffect)
import Environment (Environment(..), currentEnvironment)
import Halogen
  ( Component
  , HalogenM
  , Slot
  , get
  , getHTMLElementRef
  , getRef
  , liftEffect
  , mkComponent
  , modify_
  )
import Halogen as H
import Halogen.HTML (ComponentHTML, PlainHTML, slot, text)
import Halogen.Query.Event.Extra (eventListenerEffect)
import Halogen.Subscription as HS
import Type.Proxy (Proxy(..))
import Web.DOM.Element (DOMRect, Element, getBoundingClientRect)
import Web.Event.Event (EventType(..))
import Web.HTML (window)
import Web.HTML.HTMLDocument (toEventTarget)
import Web.HTML.Window (document)
import Web.UIEvent.MouseEvent as MouseEvent

_hintSlot :: Proxy "hintSlot"
_hintSlot = Proxy

hint
  :: forall slots m action
   . MonadAff m
  => Array String
  -> String
  -> Placement
  -> PlainHTML
  -> ComponentHTML action
       (hintSlot :: forall query. Slot query Void String | slots)
       m
hint hintElementClasses ref placement content =
  slot
    _hintSlot
    ref
    component
    { content, placement, hintElementClasses }
    absurd

initialState :: Input -> State
initialState { content, placement, hintElementClasses } =
  { content
  , placement
  , hintElementClasses
  , active: false
  , mPopperInstance: Nothing
  , mGlobalClickSubscription: Nothing
  }

component
  :: forall m query
   . MonadAff m
  => Component query Input Void m
component = case unsafePerformEffect currentEnvironment of
  -- Popper doesn't work in node, so we need to disable this component.
  NodeJs ->
    mkComponent
      { initialState
      , render: const $ text ""
      , eval: H.mkEval H.defaultEval
      }
  Browser ->
    mkComponent
      { initialState
      , render
      , eval:
          H.mkEval
            $ H.defaultEval
                { initialize = Just Init
                , finalize = Just Finalize
                , handleAction = handleAction
                , receive = \input -> Just $ OnNewInput input
                }
      }

handleAction
  :: forall m slots
   . MonadAff m
  => Action
  -> HalogenM State Action slots Void m Unit
handleAction Init = do
  placement <- use _placement
  mPopperInstance <-
    runMaybeT do
      arrowElem <- MaybeT $ getHTMLElementRef arrowRef
      modifiers <- liftEffect $ defaultModifiers <#>
        ( _ <>
            [ arrow arrowElem (PaddingValue $ pAll 0.0)
            , offset (OffsetValue { skidding: 0.0, distance: 8.0 })
            , preventOverflow defaultPreventOverflow
            , flipPlacement defaultFlip
            ]
        )
      hintElem <- MaybeT $ getHTMLElementRef hintRef
      popoutElem <- MaybeT $ getHTMLElementRef popoutRef
      liftEffect $ createPopper hintElem popoutElem
        { placement, modifiers, strategy: Fixed }
  assign _mPopperInstance mPopperInstance

handleAction Finalize = do
  mPopperInstance <- use _mPopperInstance
  for_ mPopperInstance $ liftEffect <<< destroyPopper

handleAction (OnNewInput input) = do
  { active } <- get
  modify_
    $ set _content input.content
        <<< set _placement input.placement
  when active forceUpdatePopper

handleAction Open = do
  mHintElem <- getRef hintRef
  mPopoutElem <- getRef popoutRef
  let
    mElements :: Maybe (Array Element)
    mElements = (\a b -> [ a, b ]) <$> mHintElem <*> mPopoutElem
  mGlobalClickSubscription <- traverse
    (H.subscribe <=< liftEffect <<< clickOutsideEventSource)
    mElements
  modify_
    $ set _active true
        <<< set _mGlobalClickSubscription mGlobalClickSubscription
  forceUpdatePopper

handleAction Close = do
  mGlobalClickSubscription <- use _mGlobalClickSubscription
  for_ mGlobalClickSubscription H.unsubscribe
  modify_
    $ set _active false
        <<< set _mGlobalClickSubscription Nothing

handleAction Toggle = do
  active <- use _active
  if active then
    handleAction Close
  else
    handleAction Open

type Point2D = { x :: Number, y :: Number }

outside :: DOMRect -> Point2D -> Boolean
outside { top, bottom, left, right } { x, y } = x < left || x > right
  || y
    > bottom
  || y
    < top

-- This subscription attaches to the document's click event and checks wether a click was
-- made outside the hint dialog and the hint element. We need to check for both elements
-- because we also receive the event that starts this subscription, which is inside the hint
-- element but outside the hint dialog.
-- Initially this was done using `eventListenerEventSource` which is simpler than `effectEventSource`
-- (as you don't need to manually add and remove the event listener)
-- but it had a problem of not being able to perform effects, so we couldn't recalculate
-- the client rect on each click.
clickOutsideEventSource :: Array Element -> Effect (HS.Emitter Action)
clickOutsideEventSource elements = do
  doc <- document =<< window
  pure
    $ eventListenerEffect (EventType "click") (toEventTarget doc) \evt -> do
        -- We recalculate the client rect for each element because Popper might move them
        clientRects <- for elements getBoundingClientRect
        let
          mouseEventOutside mouseEvent rect =
            outside
              rect
              { x: toNumber $ MouseEvent.clientX mouseEvent
              , y: toNumber $ MouseEvent.clientY mouseEvent
              }

          clickOutside =
            clientRects
              # filterMap
                  ( \rect ->
                      mouseEventOutside <$> MouseEvent.fromEvent evt <*> pure
                        rect
                  )
              # and
        pure $ guard clickOutside $> Close

forceUpdatePopper
  :: forall m slots. MonadAff m => HalogenM State Action slots Void m Unit
forceUpdatePopper = do
  mPopperInstance <- use _mPopperInstance
  for_ mPopperInstance $ liftEffect <<< forceUpdate
