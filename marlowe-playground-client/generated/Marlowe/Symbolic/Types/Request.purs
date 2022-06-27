-- File auto generated by purescript-bridge! --
module Marlowe.Symbolic.Types.Request where

import Prelude

import Control.Lazy (defer)
import Data.Argonaut (encodeJson, jsonNull)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Aeson ((</$\>), (</*\>), (</\>))
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Aeson ((>$<), (>/\<))
import Data.Argonaut.Encode.Aeson as E
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', iso, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple.Nested ((/\))
import Language.Marlowe.Core.V1.Semantics.Types (Contract, State)
import Type.Proxy (Proxy(Proxy))

newtype Request = Request
  { onlyAssertions :: Boolean
  , contract :: Contract
  , state :: State
  }

instance EncodeJson Request where
  encodeJson = defer \_ -> E.encode $ unwrap >$<
    ( E.record
        { onlyAssertions: E.value :: _ Boolean
        , contract: E.value :: _ Contract
        , state: E.value :: _ State
        }
    )

instance DecodeJson Request where
  decodeJson = defer \_ -> D.decode $
    ( Request <$> D.record "Request"
        { onlyAssertions: D.value :: _ Boolean
        , contract: D.value :: _ Contract
        , state: D.value :: _ State
        }
    )

derive instance Generic Request _

derive instance Newtype Request _

--------------------------------------------------------------------------------

_Request
  :: Iso' Request
       { onlyAssertions :: Boolean, contract :: Contract, state :: State }
_Request = _Newtype
