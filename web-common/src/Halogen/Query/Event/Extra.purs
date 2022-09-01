module Halogen.Query.Event.Extra where

import Prologue

import Data.Maybe (maybe)
import Effect (Effect)
import Halogen.Subscription as HS
import Web.Event.Event as Event
import Web.Event.EventTarget as EventTarget

-- | Constructs an `Emitter` for a DOM event. Accepts a function that maps event
-- | values to an effectful computation that returns a `Maybe`-wrapped action,
-- | allowing it to filter events if necessary.
eventListenerEffect
  :: forall a
   . Event.EventType
  -> EventTarget.EventTarget
  -> (Event.Event -> Effect (Maybe a))
  -> HS.Emitter a
eventListenerEffect eventType target f =
  HS.makeEmitter \push -> do
    listener <-
      EventTarget.eventListener \ev -> do
        ma <- f ev
        maybe (pure unit) push ma
    EventTarget.addEventListener eventType listener false target
    pure do
      EventTarget.removeEventListener eventType listener false target
