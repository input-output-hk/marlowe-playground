module Data.RawJson where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut.Core (fromString, stringify)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Decoders (decodeString)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)

newtype RawJson = RawJson String

derive instance genericRawJson :: Generic RawJson _

derive instance newtypeRawJson :: Newtype RawJson _

derive instance eqRawJson :: Eq RawJson

_RawJson :: Iso' RawJson String
_RawJson = _Newtype

foreign import _pretty :: String -> String

foreign import unsafeStringify :: forall a. a -> String

pretty :: RawJson -> String
pretty (RawJson str) = _pretty str

instance showRawJson :: Show RawJson where
  show = genericShow

instance encodeJsonRawJson :: EncodeJson RawJson where
  encodeJson = fromString <<< unwrap

instance decodeJsonRawJson :: DecodeJson RawJson where
  decodeJson json = map RawJson $ decodeString json <|> pure (stringify json)
