module Text.Pretty where

import Prelude

import Data.Array (intersperse)
import Data.BigInt.Argonaut (BigInt)
import Data.BigInt.Argonaut as BigInt
import Data.DateTime.Instant (Instant, unInstant)
import Data.Foldable (any, fold, intercalate)
import Data.Generic.Rep
  ( class Generic
  , Argument(..)
  , Constructor(..)
  , NoArguments
  , Product(..)
  , Sum(..)
  , from
  )
import Data.Maybe (fromJust)
import Data.Newtype (unwrap)
import Data.String.Extra (repeat)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(..))

data Doc
  = Text String
  | Newline Int
  | Cat Doc Doc
  | Empty

derive instance Eq Doc

instance Show Doc where
  show Empty = mempty
  show (Text str) = str
  show (Newline n) = "\n" <> repeat n " "
  show (Cat a b) = show a <> show b

instance Semigroup Doc where
  append = Cat

instance Monoid Doc where
  mempty = Empty

newline :: Doc
newline = Newline 0

text :: String -> Doc
text = Text

indent :: Int -> Doc -> Doc
indent n (Newline m) = Newline (m + n)

indent n (Cat a b) = Cat (indent n a) (indent n b)

indent _ doc = doc

sep :: Doc -> Array Doc -> Doc
sep separator = fold <<< intersperse separator

hsep :: Array Doc -> Doc
hsep = sep $ text " "

vsep :: Array Doc -> Doc
vsep = sep $ newline

------------------------ Pretty Class and Generic Instances ---------------------------
class Pretty a where
  pretty :: a -> Doc

instance prettyString :: Pretty String where
  pretty = text <<< show

instance prettyInstant :: Pretty Instant where
  pretty instant = pretty
    $ unsafePartial
    $ fromJust
    $ BigInt.fromNumber
    $ unwrap
    $ unInstant instant

instance prettyBigInt :: Pretty BigInt where
  pretty = text <<< BigInt.toString

instance prettyNoArguments :: Pretty NoArguments where
  pretty _ = mempty

instance prettyArray :: Pretty a => Pretty (Array a) where
  pretty xs =
    let
      docs = map pretty xs

      list = intercalate (text ", ") docs
    in
      text "[" <> list <> text "]"

instance prettySum :: (Pretty l, Pretty r) => Pretty (Sum l r) where
  pretty (Inl l) = pretty l
  pretty (Inr r) = pretty r

instance prettyArgument :: (Pretty a, Args a) => Pretty (Argument a) where
  pretty (Argument a) =
    if hasArgs a then
      text "(" <> pretty a <> (if hasNestedArgs a then newline else mempty) <>
        text ")"
    else
      pretty a

instance prettyProduct ::
  ( GenericArgs l
  , GenericArgs r
  , Pretty l
  , Pretty r
  ) =>
  Pretty (Product l r) where
  pretty (Product l r) = pretty l
    <> (if (gHasArgs l || gHasArgs r) then newline else text " ")
    <> pretty r

instance prettyConstructor ::
  ( GenericArgs a
  , Pretty a
  , IsSymbol name
  ) =>
  Pretty (Constructor name a) where
  pretty (Constructor a) =
    if gHasNestedArgs a then
      text conName <> indent 4 (newline <> pretty a)
    else
      text conName <> text " " <> pretty a
    where
    conName :: String
    conName = reflectSymbol (Proxy :: _ name)

genericPretty :: forall a rep. Generic a rep => Pretty rep => a -> Doc
genericPretty x = pretty (from x)

class Args a where
  hasArgs :: a -> Boolean
  hasNestedArgs :: a -> Boolean

instance hasArgsString :: Args String where
  hasArgs _ = false
  hasNestedArgs _ = false

instance hasArgsBigInt :: Args BigInt where
  hasArgs _ = false
  hasNestedArgs _ = false

instance Args Instant where
  hasArgs _ = false
  hasNestedArgs _ = false

instance argsArray :: Args a => Args (Array a) where
  hasArgs _ = false
  hasNestedArgs as = any hasNestedArgs as

class GenericArgs a where
  gHasArgs :: a -> Boolean
  gHasNestedArgs :: a -> Boolean

instance argsSum :: (GenericArgs l, GenericArgs r) => GenericArgs (Sum l r) where
  gHasArgs (Inl l) = gHasArgs l
  gHasArgs (Inr r) = gHasArgs r
  gHasNestedArgs (Inl l) = gHasNestedArgs l
  gHasNestedArgs (Inr r) = gHasNestedArgs r

instance argsNoArguments :: GenericArgs NoArguments where
  gHasArgs _ = false
  gHasNestedArgs _ = false

instance argsArgument :: Args a => GenericArgs (Argument a) where
  gHasArgs (Argument a) = hasArgs a
  gHasNestedArgs (Argument a) = hasNestedArgs a

instance argsGeneric1 :: Args a => GenericArgs (Constructor name (Argument a)) where
  gHasArgs (Constructor (Argument _)) = true
  gHasNestedArgs (Constructor (Argument _)) = false

instance argsGeneric2 :: GenericArgs (Constructor name NoArguments) where
  gHasArgs (Constructor _) = false
  gHasNestedArgs (Constructor _) = false

instance argsProdcutArgs ::
  ( Args a
  , Args b
  ) =>
  GenericArgs (Product (Argument a) (Argument b)) where
  gHasArgs (Product (Argument _) (Argument _)) = true
  gHasNestedArgs (Product (Argument l) (Argument r)) = hasArgs l || hasArgs r
else instance argsProdcut ::
  ( Args a
  , GenericArgs b
  ) =>
  GenericArgs (Product (Argument a) b) where
  gHasArgs (Product _ _) = true
  gHasNestedArgs (Product (Argument l) r) = hasArgs l || gHasArgs r

instance argsGeneric ::
  ( Args a
  , Args b
  ) =>
  GenericArgs (Constructor name (Product (Argument a) (Argument b))) where
  gHasArgs (Constructor (Product _ _)) = true
  gHasNestedArgs (Constructor (Product (Argument l) (Argument r))) = hasArgs l
    || hasArgs r
else instance argsGenericProdcutL ::
  ( Args a
  , GenericArgs b
  ) =>
  GenericArgs (Constructor name (Product (Argument a) b)) where
  gHasArgs (Constructor (Product _ _)) = true
  gHasNestedArgs (Constructor (Product (Argument l) r)) = hasArgs l || gHasArgs
    r

genericHasArgs :: forall a rep. Generic a rep => GenericArgs rep => a -> Boolean
genericHasArgs = gHasArgs <<< from

genericHasNestedArgs
  :: forall a rep. Generic a rep => GenericArgs rep => a -> Boolean
genericHasNestedArgs = gHasNestedArgs <<< from
