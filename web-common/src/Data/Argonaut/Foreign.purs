module Data.Argonaut.Foreign where

import Prologue

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Lens (Iso', iso)
import Foreign (Foreign)
import Unsafe.Coerce (unsafeCoerce)

toForeign :: Json -> Foreign
toForeign = unsafeCoerce

fromForeign :: Foreign -> Json
fromForeign = unsafeCoerce

_Json :: Iso' Json Foreign
_Json = iso toForeign fromForeign

decodeForeign
  :: forall a. DecodeJson a => Foreign -> Either JsonDecodeError a
decodeForeign = decodeJson <<< fromForeign

encodeForeign
  :: forall a. EncodeJson a => a -> Foreign
encodeForeign = encodeJson >>> toForeign
