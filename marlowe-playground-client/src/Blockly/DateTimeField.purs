module Blockly.DateTimeField where

import Prelude

import Blockly.Types (Blockly)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)

foreign import registerDateTimeFieldImpl :: EffectFn1 Blockly Unit

registerDateTimeField :: Blockly -> Effect Unit
registerDateTimeField = runEffectFn1 registerDateTimeFieldImpl

