module Analytics
  ( Event
  , SegmentEvent
  , defaultEvent
  , class IsEvent
  , toEvent
  , analyticsTracking
  ) where

import Prelude

import Control.Plus (empty)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Encode.Aeson as E
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Data.Tuple.Nested ((/\))
import Data.UndefinedOr (UndefinedOr, toUndefined)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, EffectFn4, runEffectFn2, runEffectFn4)
import Foreign.Object (Object)
import Foreign.Object as Object

foreign import trackEvent_
  :: EffectFn4
       String
       (UndefinedOr String)
       (UndefinedOr String)
       (UndefinedOr Number)
       Unit

foreign import trackSegmentEvent_
  :: EffectFn2 String (Object Json) Unit

type Event =
  { action :: String
  , category :: Maybe String
  , label :: Maybe String
  , value :: Maybe Number
  }

type SegmentEvent =
  { action :: String
  , payload :: Object Json
  }

type TimingEvent =
  { category :: String
  , variable :: String
  , miliseconds :: Number
  , label :: String
  }

defaultEvent :: String -> Event
defaultEvent action =
  { action
  , category: Nothing
  , label: Nothing
  , value: Nothing
  }

trackEvent :: Event -> Effect Unit
trackEvent { action, category, label, value } =
  runEffectFn4 trackEvent_
    action
    (maybeToUndefined category)
    (maybeToUndefined label)
    (maybeToUndefined value)
  where
  maybeToUndefined :: forall a. Maybe a -> UndefinedOr a
  maybeToUndefined Nothing = empty
  maybeToUndefined (Just a) = toUndefined a

trackSegmentEvent :: SegmentEvent -> Effect Unit
trackSegmentEvent { action, payload } = runEffectFn2 trackSegmentEvent_ action
  payload

class IsEvent a where
  toEvent :: a -> Maybe Event

toSegmentEvent :: Event -> SegmentEvent
toSegmentEvent { action, category, label, value } =
  { action
  , payload:
      Object.fromFoldable
        [ "category" /\ E.encode (E.maybe E.value) category
        , "label" /\ E.encode (E.maybe E.value) label
        , "value" /\ E.encode (E.maybe E.value) value
        ]
  }

analyticsTracking :: forall a. IsEvent a => a -> Effect Unit
analyticsTracking action = do
  for_ (toEvent action)
    $ \event -> do
        trackEvent event
        trackSegmentEvent $ toSegmentEvent event
