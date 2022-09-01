module PlutusTx.AssocMap where

import Prologue

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Array as Array
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.FoldableWithIndex (class FoldableWithIndex)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', lens)
import Data.Lens.AffineTraversal (affineTraversal)
import Data.Lens.At (class At)
import Data.Lens.Index (class Index)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Map as Data.Map
import Data.Newtype (class Newtype, unwrap)
import Data.Newtype as Newtype
import Data.Set (Set)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Tuple (uncurry)

newtype Map a b = Map (Array (Tuple a b))

keys :: forall k v. Ord k => Map k v -> Set k
keys (Map entries) = Set.fromFoldable $ map (fst) entries

derive instance functorMap :: Functor (Map a)

derive newtype instance encodeJsonMap ::
  ( EncodeJson a
  , EncodeJson b
  ) =>
  EncodeJson (Map a b)

derive newtype instance decodeJsonMap ::
  ( DecodeJson a
  , DecodeJson b
  ) =>
  DecodeJson (Map a b)

derive instance genericMap :: Generic (Map a b) _

derive instance newtypeMap :: Newtype (Map a b) _

instance eqMap :: (Ord k, Eq k, Eq v) => Eq (Map k v) where
  eq = eq `on` toDataMap

--------------------------------------------------------------------------------
_Map :: forall a b. Iso' (Map a b) (Array (Tuple a b))
_Map = _Newtype

instance showMap :: (Show k, Show v) => Show (Map k v) where
  show x = genericShow x

instance foldableMap :: Foldable (Map k) where
  foldMap f = foldMap (f <<< snd) <<< unwrap
  foldl f z = foldl (\b -> f b <<< snd) z <<< unwrap
  foldr f z = foldr (\x b -> f (snd x) b) z <<< unwrap

instance foldableWithIndexMap :: FoldableWithIndex k (Map k) where
  foldMapWithIndex f = foldMap (uncurry f) <<< unwrap
  foldlWithIndex f z = foldl (\acc (Tuple k v) -> f k acc v) z <<< unwrap
  foldrWithIndex f z = foldr (\(Tuple k v) acc -> f k v acc) z <<< unwrap

instance indexMap :: Eq k => Index (Map k a) k a where
  ix key = affineTraversal set pre
    where
    set :: Map k a -> a -> Map k a
    set m v =
      Newtype.over
        Map
        (map \(Tuple k v') -> Tuple k $ if k == key then v else v')
        m

    pre :: Map k a -> Either (Map k a) a
    pre (Map map) = case Array.find ((_ == key) <<< fst) map of
      Just (Tuple _ a) -> Right a
      Nothing -> Left $ Map map

instance atMap :: Eq k => At (Map k a) k a where
  at key = lens get set
    where
    matching tuple = fst tuple == key

    get (Map xs) = snd <$> Array.find matching xs

    set (Map xs) Nothing = Map $ Array.filter (not matching) $ xs

    set (Map xs) (Just new) =
      Map
        $ case Array.findIndex matching xs of
            Nothing -> Array.snoc xs (Tuple key new)
            _ ->
              map
                (\(Tuple k v) -> Tuple k (if k == key then new else v))
                xs

null :: forall k v. Map k v -> Boolean
null = Array.null <<< unwrap

-- | Compute the union of two `AssocMaps`s, using the specified function to combine values for duplicate keys.
-- Notes:
--
--   * This function does not offer any guarantees about ordering.
--   * `AssocMap`s may themselves contain duplicate keys, and so the
--     same key may appear several times in *both* input `AssocMap`s. They
--     too will be combined with the given function. Thus the
--     resulting `AssocMap` will have unique keys.
unionWith :: forall k v. Ord k => (v -> v -> v) -> Map k v -> Map k v -> Map k v
unionWith f a b =
  Map
    $ Data.Map.toUnfoldable
    $ on (Data.Map.unionWith f) (Data.Map.fromFoldableWith f <<< unwrap)
        a
        b

instance semigroupMap :: (Ord k, Semigroup v) => Semigroup (Map k v) where
  append = unionWith append

instance monoidMap :: (Ord k, Semigroup v) => Monoid (Map k v) where
  mempty = Map mempty

toDataMap :: forall k v. Ord k => Map k v -> Data.Map.Map k v
toDataMap = Data.Map.fromFoldable <<< unwrap
