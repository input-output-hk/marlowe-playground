module Web.Blob.Clipboard (copyToClipboard) where

import Prelude

import Control.Promise (Promise, toAffE)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn1, runEffectFn1)

foreign import copyToClipboardImpl :: EffectFn1 String (Promise Unit)

copyToClipboard :: String -> Aff Unit
copyToClipboard str = toAffE (runEffectFn1 copyToClipboardImpl str)
