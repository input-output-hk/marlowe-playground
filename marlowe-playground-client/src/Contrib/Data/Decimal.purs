module Contrib.Data.Decimal where

import Prelude

import Data.BigInt.Argonaut (BigInt)
import Data.BigInt.Argonaut as BI
import Data.Decimal (Decimal)
import Data.Decimal as D
import Data.Maybe (Maybe(..))
import Data.Numbers.Natural (Natural)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Partial.Unsafe (unsafeCrashWith)
import Unsafe.Coerce (unsafeCoerce)

newtype Precision = Precision Natural

foreign import data Rounding :: Type

foreign import _ROUND_UP :: Rounding
foreign import _ROUND_DOWN :: Rounding

instance Eq Rounding where
  eq r1 r2 = do
    let
      r1' = unsafeCoerce r1 :: Int
      r2' = unsafeCoerce r2 :: Int
    r1' == r2'

-- Unfortunatelly Decimal.js uses global setup which drives parsing
-- of the values. We should probably wrap every 
-- http://mikemcl.github.io/decimal.js/#constructor-properties
type Configuration = { precision :: Precision, rounding :: Rounding }

foreign import setConfigurationImpl :: EffectFn1 Configuration Unit

setConfiguration :: Configuration -> Effect Unit
setConfiguration = runEffectFn1 setConfigurationImpl

foreign import getConfiguration :: Effect Configuration

-- | Please be aware that this can round the value according to a global setup of
-- | decimal.js constructor (precision and rounding)!
fromBigInt :: BigInt -> Decimal
fromBigInt i = case D.fromString (BI.toString i) of
  Just d -> d
  Nothing -> unsafeCrashWith $ "Decimal.fromBigInt` fails for a given input:" <>
    BI.toString i

toBigInt :: Decimal -> BigInt
toBigInt d = do
  let
    dString = D.toFixed zero d
  case BI.fromString dString of
    Just i -> i
    Nothing -> unsafeCrashWith $ "Decimal.toBigInt` fails for a given input:" <>
      dString
