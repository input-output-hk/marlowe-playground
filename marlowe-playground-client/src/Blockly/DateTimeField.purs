module Blockly.DateTimeField where

import Blockly.Types (Blockly)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)

foreign import data FieldDateTime :: Type

foreign import registerDateTimeFieldImpl :: EffectFn1 Blockly FieldDateTime

registerDateTimeField :: Blockly -> Effect FieldDateTime
registerDateTimeField = runEffectFn1 registerDateTimeFieldImpl

