module Halogen.HTML.Events.Extra where

import Prologue

import Halogen.HTML (IProp)
import Halogen.HTML.Events (onBlur, onClick, onFocus, onValueInput)
import Web.Event.Internal.Types (Event)
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.MouseEvent (MouseEvent)

onClick_ :: forall a b. a -> IProp (onClick :: MouseEvent | b) a
onClick_ = onClick <<< const

onValueInput_
  :: forall a b
   . (String -> a)
  -> IProp (onInput :: Event, value :: String | b) a
onValueInput_ = onValueInput

onFocus_ :: forall a b. a -> IProp (onFocus :: FocusEvent | b) a
onFocus_ = onFocus <<< const

onBlur_ :: forall a b. a -> IProp (onBlur :: FocusEvent | b) a
onBlur_ = onBlur <<< const
