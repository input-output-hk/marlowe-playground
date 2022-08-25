module Halogen.ElementResize where

import Prelude

import Data.Array (head)
import Data.Traversable (traverse_)
import Halogen.Subscription as HS
import Web.DOM (Element)
import Web.DOM.ResizeObserver
  ( ResizeObserverBoxOptions
  , ResizeObserverEntry
  , observe
  , resizeObserver
  , unobserve
  )

elementResize
  :: forall action
   . ResizeObserverBoxOptions
  -> (ResizeObserverEntry -> action)
  -> Element
  -> HS.Emitter action
elementResize options toAction element =
  HS.makeEmitter \push -> do
    observer <-
      resizeObserver \entries _ ->
        traverse_ (push <<< toAction) $ head entries
    observe element options observer
    pure $ unobserve element observer
