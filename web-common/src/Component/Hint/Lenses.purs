module Component.Hint.Lenses where

import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Type.Proxy (Proxy(..))

_content :: forall s a. Lens' { content :: a | s } a
_content = prop (Proxy :: _ "content")

_active :: forall s a. Lens' { active :: a | s } a
_active = prop (Proxy :: _ "active")

_reference :: forall s a. Lens' { reference :: a | s } a
_reference = prop (Proxy :: _ "reference")

_placement :: forall s a. Lens' { placement :: a | s } a
_placement = prop (Proxy :: _ "placement")

_mPopperInstance :: forall s a. Lens' { mPopperInstance :: a | s } a
_mPopperInstance = prop (Proxy :: _ "mPopperInstance")

_mGlobalClickSubscription
  :: forall s a. Lens' { mGlobalClickSubscription :: a | s } a
_mGlobalClickSubscription = prop (Proxy :: _ "mGlobalClickSubscription")
