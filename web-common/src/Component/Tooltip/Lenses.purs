module Component.Tooltip.Lenses where

import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Type.Proxy (Proxy(..))

_message :: forall s a. Lens' { message :: a | s } a
_message = prop (Proxy :: _ "message")

_active :: forall s a. Lens' { active :: a | s } a
_active = prop (Proxy :: _ "active")

_reference :: forall s a. Lens' { reference :: a | s } a
_reference = prop (Proxy :: _ "reference")

_placement :: forall s a. Lens' { placement :: a | s } a
_placement = prop (Proxy :: _ "placement")

_mPopperInstance :: forall s a. Lens' { mPopperInstance :: a | s } a
_mPopperInstance = prop (Proxy :: _ "mPopperInstance")
