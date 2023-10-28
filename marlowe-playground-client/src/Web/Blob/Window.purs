module Web.Blob.Window (getUrl) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)

foreign import getUrlImpl :: EffectFn1 Unit String

getUrl :: Effect String
getUrl = runEffectFn1 getUrlImpl unit
