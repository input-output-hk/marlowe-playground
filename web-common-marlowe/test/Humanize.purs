module Test.Humanize where

import Prologue

import Data.BigInt.Argonaut as BigInt
import Data.DateTime (time)
import Data.DateTime.Instant (instant, toDateTime)
import Data.Enum (toEnum)
import Data.Maybe (fromJust)
import Data.Time (hour)
import Data.Time.Duration (Milliseconds(..), Minutes(..))
import Data.Tuple.Nested ((/\))
import Humanize
  ( formatInstant
  , humanizeOffset
  , humanizeValue
  , localToUtc
  , utcToLocal
  )
import Language.Marlowe.Core.V1.Semantics.Types (Token(..), adaToken)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

all :: Spec Unit
all = do
  -- TODO: humanizeDurationSpec, humanizeIntervalSpec
  humanizeValueSpec
  humanizeOffsetSpec
  formatInstantSpec
  utcToLocalSpec
  localToUtcSpec

humanizeValueSpec :: Spec Unit
humanizeValueSpec = describe "humanizeValue" do
  describe "ADA" do
    it "cents should be displayed with 6 digits" do
      humanizeValue adaToken (BigInt.fromInt 100) `shouldEqual` "₳ 0.000100"
      humanizeValue adaToken (BigInt.fromInt 1_500_000) `shouldEqual`
        "₳ 1.500000"
    it "full numbers should not have commas" do
      humanizeValue adaToken (BigInt.fromInt 0) `shouldEqual` "₳ 0"
      humanizeValue adaToken (BigInt.fromInt 1_000_000) `shouldEqual` "₳ 1"
      humanizeValue adaToken (BigInt.fromInt 2_000_000) `shouldEqual` "₳ 2"
  describe "Dolar" do
    let dolarToken = Token "" "dollar"

    it "should always use 2 centimal position" do
      humanizeValue dolarToken (BigInt.fromInt 0) `shouldEqual` "$ 0.00"
      humanizeValue dolarToken (BigInt.fromInt 10_00) `shouldEqual` "$ 10.00"
      humanizeValue dolarToken (BigInt.fromInt 15_20) `shouldEqual` "$ 15.20"
  describe "Custom token" do
    let customToken = Token "" "Monkey NFT"
    it "should not use decimals and it should suffix the token name" do
      humanizeValue customToken (BigInt.fromInt 1) `shouldEqual` "1 Monkey NFT"
      humanizeValue customToken (BigInt.fromInt 524) `shouldEqual`
        "524 Monkey NFT"

humanizeOffsetSpec :: Spec Unit
humanizeOffsetSpec = describe "humanizeOffset" do
  it "should display GMT if there is no offset" do
    humanizeOffset (Minutes zero) `shouldEqual` "GMT"
  it "should display a negative offset for countries western that greenwich" do
    -- See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/getTimezoneOffset#negative_values_and_positive_values
    humanizeOffset (Minutes 180.0) `shouldEqual` "GMT-3"
  it "should display a positive offset for countries eastern that greenwich" do
    -- See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/getTimezoneOffset#negative_values_and_positive_values
    humanizeOffset (Minutes (-600.0)) `shouldEqual` "GMT+10"
  it "should display minutes when the offset is not a full hour" do
    humanizeOffset (Minutes 90.0) `shouldEqual` "GMT-1:30"

formatInstantSpec :: Spec Unit
formatInstantSpec = describe "formatInstant" do
  -- Wednesday, April 27, 2022 7:57:26 PM GMT
  let
    someUTCTime = unsafePartial $ fromJust $ instant $ Milliseconds
      1651089446000.0
  it "should not modify the date if the offset is zero" do
    formatInstant (Minutes zero) someUTCTime `shouldEqual`
      ("27 Apr 2022" /\ "19:57")
  it "should substract the time for positive offset" do
    formatInstant (Minutes 180.0) someUTCTime `shouldEqual`
      ("27 Apr 2022" /\ "16:57")
  it "should add the time for negative offset" do
    formatInstant (Minutes (-60.0)) someUTCTime `shouldEqual`
      ("27 Apr 2022" /\ "20:57")
  it "should modify the date if the time passes 24 hours" do
    formatInstant (Minutes (-720.0)) someUTCTime `shouldEqual`
      ("28 Apr 2022" /\ "07:57")

utcToLocalSpec :: Spec Unit
utcToLocalSpec = describe "utcToLocal" do
  let
    -- Wednesday, April 27, 2022 7:57:26 PM GMT
    someUTCDateTime = toDateTime $ unsafePartial $ fromJust $ instant $
      Milliseconds 1651089446000.0
    hour' = Just <<< hour <<< time
  it "should not modify the date if the offset is zero" do
    utcToLocal (Minutes zero) someUTCDateTime `shouldEqual` someUTCDateTime
  it "should substract the time for positive offset" do
    (hour' $ utcToLocal (Minutes 180.0) someUTCDateTime) `shouldEqual` toEnum 16
  it "should add the time for negative offset" do
    (hour' $ utcToLocal (Minutes (-60.0)) someUTCDateTime) `shouldEqual` toEnum
      20

localToUtcSpec :: Spec Unit
localToUtcSpec = describe "localToUtc" do
  let
    -- Wednesday, April 27, 2022 4:57:26 PM GMT
    someUTCDateTime = toDateTime $ unsafePartial $ fromJust $ instant $
      Milliseconds 1651089446000.0
    tzOffset = Minutes 180.0
    someLocalDateTime = utcToLocal tzOffset someUTCDateTime
  -- This could be a property based test, but is probably not worth it.
  it "localToUtc <<< utcToLocal should not modify the time" do
    localToUtc tzOffset someLocalDateTime `shouldEqual` someUTCDateTime
