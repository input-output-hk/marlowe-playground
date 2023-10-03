module Data.Numbers.Natural where

import Prelude

import Data.Enum (class Enum)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafeCrashWith)

newtype Natural = Natural Int

derive newtype instance Eq Natural
derive newtype instance Ord Natural
derive newtype instance Enum Natural
derive newtype instance Semiring Natural

fromInt :: Int -> Maybe Natural
fromInt x | x > 0 = Just $ Natural x
fromInt _ = Nothing

unsafeFromInt :: String -> Int -> Natural
unsafeFromInt msg i = case fromInt i of
  Just n -> n
  Nothing -> unsafeCrashWith $ "unsafeNatural:" <> msg

toInt :: Natural -> Int
toInt (Natural x) = x

