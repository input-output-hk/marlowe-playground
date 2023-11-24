module Text.Bech32 (validPaymentShelleyAddress) where

import Prelude

import Control.Alternative (guard)
import Data.Array (range, uncons, (!!))
import Data.Char (toCharCode)
import Data.Foldable (all, foldl)
import Data.Int.Bits (shl, shr, xor, (.&.), (.|.))
import Data.Maybe (Maybe, fromMaybe)
import Data.String (Pattern(..), lastIndexOf, length, splitAt, stripSuffix)
import Data.String.CodeUnits (fromCharArray, indexOf, toCharArray)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))

gen :: Array Int
gen = [ 0x3b6a57b2, 0x26508e6d, 0x1ea119fa, 0x3d4233dd, 0x2a1462b3 ]

charTable :: String
charTable = "qpzry9x8gf2tvdw0s3jn54khce6mua7l"

bech32Polymod :: Array Int -> Int
bech32Polymod values = foldl go 1 values
  where
  go chk v =
    let
      b = shr chk 25
    in
      foldl
        ( \acc i ->
            acc `xor`
              ( if (shr b i .&. 1) == 1 then fromMaybe 0 (gen !! i)
                else 0
              )
        )
        (shl (chk .&. 0x1ffffff) 5 `xor` v)
        (range 0 4)

bech32HrpExpand :: String -> Array Int
bech32HrpExpand s = map (\c -> shr (toCharCode c) 5) (toCharArray s) <> [ 0 ] <>
  map (\c -> toCharCode c .&. 31) (toCharArray s)

bech32VerifyChecksum :: String -> Array Int -> Boolean
bech32VerifyChecksum hrp data' = bech32Polymod (bech32HrpExpand hrp <> data') ==
  1

stringToIntArray :: String -> Maybe (Array Int)
stringToIntArray s = traverse go (toCharArray s)
  where
  go :: Char -> Maybe Int
  go c = indexOf (Pattern (fromCharArray [ c ])) charTable

validCharacters :: String -> Boolean
validCharacters s = all ((\c -> c >= 33 && c <= 126) <<< toCharCode)
  (toCharArray s)

splitAddress :: String -> Maybe (String /\ Array Int)
splitAddress s = do
  p <- lastIndexOf (Pattern "1") s
  let { before: hrp1, after: data' } = splitAt (p + 1) s
  hrp <- stripSuffix (Pattern "1") hrp1
  let
    hrpl = length hrp
    datal = length data'
  intDataArr <- stringToIntArray data'
  guard
    ( hrpl >= 1 && validCharacters hrp && datal >= 6 && bech32VerifyChecksum hrp
        intDataArr
    )
  pure $ (hrp /\ intDataArr)

validPaymentShelleyAddress :: String -> Boolean
validPaymentShelleyAddress sAddr =
  fromMaybe false
    do
      (pref /\ dat) <- splitAddress sAddr
      { head: b_0_5, tail: t } <- uncons dat -- First 5 bits
      { head: b_6_10, tail: _ } <- uncons t -- Second 5 bits
      let
        addrType = shr b_0_5 1
        network = (shl (b_0_5 .&. 0x01) 3) .|. (shr b_6_10 2)
      pure $
        ( (pref == "addr_test" && network == 0) ||
            (pref == "addr" && network == 1)
        ) -- Header matches network

          && addrType <= 7 -- It is a payment key (not a stake address)
          && addrType .&. 0x01 == 0 -- It is not a script address
