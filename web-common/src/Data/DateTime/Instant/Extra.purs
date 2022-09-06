module Data.DateTime.Instant.Extra where

import Prelude

import Data.BigInt.Argonaut (BigInt)
import Data.BigInt.Argonaut as BigInt
import Data.DateTime.Instant
  ( Instant
  , instant
  , unInstant
  )
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (unwrap)
import Data.Time.Duration (Milliseconds(..))
import Partial.Unsafe (unsafePartial)

instantToBigInt :: Instant -> BigInt
instantToBigInt time =
  unsafePartial $ fromJust $ BigInt.fromNumber $ unwrap $ unInstant time

bigIntToInstant :: BigInt -> Maybe Instant
bigIntToInstant = instant <<< Milliseconds <<< BigInt.toNumber
