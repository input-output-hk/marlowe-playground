-- As 'POSIXTime' from 'Plutus.V1.Ledger.Time' has a custom 'FromJSON' and 'ToJSON'
-- instances, we declare the corresponding 'POSIXTime' PureScript type here since
-- the PSGenerator Generic instances won't match.
module Plutus.V1.Ledger.Time where

import Prologue

import Data.Argonaut (JsonDecodeError(..), decodeJson, encodeJson)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.BigInt.Argonaut (BigInt)
import Data.BigInt.Argonaut as BigInt
import Data.DateTime (DateTime)
import Data.DateTime as DT
import Data.DateTime.Instant
  ( Instant
  , fromDateTime
  , instant
  , toDateTime
  , unInstant
  )
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromJust)
import Data.Newtype (class Newtype, over, traverse, unwrap)
import Data.Show.Generic (genericShow)
import Data.Time.Duration
  ( class Duration
  , Milliseconds(..)
  , Minutes(..)
  , convertDuration
  )
import Partial.Unsafe (unsafePartial)
import Text.Pretty (class Args, class Pretty)

-- TODO: try to delete this file
newtype POSIXTime = POSIXTime Instant

derive instance Newtype POSIXTime _
derive instance Generic POSIXTime _
derive instance Eq POSIXTime
derive instance Ord POSIXTime
derive newtype instance Bounded POSIXTime

instance Show POSIXTime where
  show x = genericShow x

instance EncodeJson POSIXTime where
  encodeJson = encodeJson <<< toBigInt

instance DecodeJson POSIXTime where
  decodeJson = (note (TypeMismatch "POSIXTime") <<< fromBigInt) <=< decodeJson

derive newtype instance Pretty POSIXTime
instance Args POSIXTime where
  hasArgs _ = false
  hasNestedArgs _ = false

fromBigInt :: BigInt -> Maybe POSIXTime
fromBigInt = map POSIXTime <<< instant <<< Milliseconds <<< BigInt.toNumber

toBigInt :: POSIXTime -> BigInt
toBigInt (POSIXTime time) =
  unsafePartial $ fromJust $ BigInt.fromNumber $ unwrap $ unInstant time

adjust :: forall d. Duration d => d -> POSIXTime -> Maybe POSIXTime
adjust d = traverse POSIXTime $ map fromDateTime <<< DT.adjust d <<< toDateTime

fromDuration :: forall d. Duration d => d -> Maybe POSIXTime
fromDuration = map POSIXTime <<< instant <<< convertDuration

toDuration :: forall d. Duration d => POSIXTime -> d
toDuration = convertDuration <<< unInstant <<< unwrap

toLocalDateTime :: Minutes -> POSIXTime -> Maybe DateTime
toLocalDateTime tzOffset = DT.adjust (over Minutes negate tzOffset :: Minutes)
  <<< toDateTime
  <<< unwrap

fromLocalDateTime :: Minutes -> DateTime -> Maybe POSIXTime
fromLocalDateTime tzOffset =
  map (POSIXTime <<< fromDateTime) <<< DT.adjust tzOffset
