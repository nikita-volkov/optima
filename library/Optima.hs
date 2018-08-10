module Optima
(
  -- * IO
  params,
  -- * Params
  Params,
  param,
  -- * Param
  Param,
  value,
  -- * Value
  Value,
  explicitParser,
  implicitParser,
  -- * Default
  Default,
  explicitlyRepresented,
  showable,
  defaultless,
  -- * ValueFormat
  ValueFormat,
  enum,
  unformatted,
)
where

import Optima.Prelude
import qualified Data.Text as Text
import qualified Data.Attoparsec.Text as Attoparsec
import qualified Options.Applicative as Optparse
import qualified Attoparsec.Data as Attoparsec
import qualified Text.Builder as TextBuilder


-- * Types
-------------------------

{-|
Parameters product parser.
Should be used for composition of all application parameters.
-}
newtype Params a = Params (Optparse.Parser a)

{-|
Parameter parser.

Includes the description of the parameter.
-}
newtype Param a = Param (Maybe Char -> Text -> Optparse.Parser a)

{-|
Parameter value parser.
-}
newtype Value a = Value (Attoparsec.Parser a)

{-|
Default value with its textual representation.
-}
data Default a = SpecifiedDefault a Text | UnspecifiedDefault

{-|
Parameter description.
-}
data ValueFormat a = EnumValueFormat [TextBuilder.Builder] | UnspecifiedFormat


-- * Instances
-------------------------

deriving instance Functor Params
deriving instance Applicative Params
deriving instance Alternative Params

deriving instance Functor Param

deriving instance Functor Value
deriving instance Applicative Value
deriving instance Alternative Value
deriving instance Monad Value
deriving instance MonadPlus Value
deriving instance MonadFail Value

deriving instance Functor Default

deriving instance Functor ValueFormat


-- * Functions
-------------------------

-- ** IO
-------------------------

{-|
Execute the parameters parser in IO,
throwing an exception if anything goes wrong.
-}
params :: Text {-^ Description of the application -} -> Params a -> IO a
params description (Params parser) =
  Optparse.execParser (Optparse.info (Optparse.helper <*> parser) mods)
  where
    mods = Optparse.fullDesc <> Optparse.progDesc (Text.unpack description)


-- ** Param
-------------------------

{-|
Lift a single parameter parser.
-}
param :: Maybe Char {-^ Single-char name -} -> Text {-^ Long name -} -> Param a -> Params a
param shortName longName (Param parser) = Params (parser shortName longName)


-- ** Param
-------------------------

{-|
Create a single parameter parser from a value parser and meta information.
-}
value :: Text {-^ Description. Can be empty -} -> Default a {-^ Default value -} -> ValueFormat a {-^ Value format -} -> Value a -> Param a
value description def format (Value attoparsecParser) =
  Param (\ shortName longName -> Optparse.option readM (mods shortName longName))
  where
    readM = Optparse.eitherReader (Attoparsec.parseOnly attoparsecParser . Text.pack)
    mods shortName longName =
      longParamName longName <>
      foldMap Optparse.short shortName <>
      paramHelp description format <>
      defaultValue def


-- ** Value
-------------------------

{-|
Lift an Attoparsec parser into value parser.
-}
explicitParser :: Attoparsec.Parser a -> Value a
explicitParser = Value

{-|
Lift an implicit lenient Attoparsec parser into value parser.
-}
implicitParser :: Attoparsec.LenientParser a => Value a
implicitParser = Value Attoparsec.lenientParser


-- ** Default
-------------------------

{-|
Provide a default value with explicit textual representation.
-}
explicitlyRepresented :: a -> Text -> Default a
explicitlyRepresented value representation = SpecifiedDefault value representation

{-|
Provide a default value with textual representation formed using the implicit Show instance.
-}
showable :: Show a => a -> Default a
showable a = SpecifiedDefault a (Text.pack (show a))

{-|
Provide no default value.
-}
defaultless :: Default a
defaultless = UnspecifiedDefault


-- ** Value spec
-------------------------

{-|
Derive value format specification from the Enum instance.
-}
enum :: (Bounded a, Enum a, Show a) => ValueFormat a
enum = let
  values = enumFromTo minBound (asTypeOf maxBound (descriptionToA description))
  descriptionToA = undefined :: ValueFormat a -> a
  description = EnumValueFormat (fmap (TextBuilder.string . show) values)
  in description

{-|
Avoid specifying the format.
-}
unformatted :: ValueFormat a
unformatted = UnspecifiedFormat


-- ** Rendering building
-------------------------

buildValueFormat :: ValueFormat a -> TextBuilder.Builder
buildValueFormat = \ case
  EnumValueFormat values -> "(" <> TextBuilder.intercalate ", " values <> ")"
  UnspecifiedFormat -> mempty

buildHelp :: Text -> ValueFormat a -> TextBuilder.Builder
buildHelp description valueFormat =
  TextBuilder.intercalate (TextBuilder.char ' ')
    (notNull (TextBuilder.text description) <> notNull (buildValueFormat valueFormat))
  where
    notNull :: TextBuilder.Builder -> [TextBuilder.Builder]
    notNull = validate (not . TextBuilder.null)


-- ** Rendering
-------------------------

renderIfNotEmpty :: TextBuilder.Builder -> Maybe Text
renderIfNotEmpty = fmap TextBuilder.run . validate (not . TextBuilder.null)


-- ** Mods
-------------------------

paramHelp :: Text -> ValueFormat a -> Optparse.Mod f a
paramHelp description format =
  foldMap (Optparse.help . Text.unpack) (renderIfNotEmpty (buildHelp description format))

defaultValue :: Optparse.HasValue f => Default a -> Optparse.Mod f a
defaultValue = \ case
  SpecifiedDefault a text -> Optparse.value a <> Optparse.showDefaultWith (const (Text.unpack text))
  UnspecifiedDefault -> mempty

longParamName :: Optparse.HasName f => Text -> Optparse.Mod f a
longParamName name =
  maybe mempty (Optparse.long . Text.unpack) (validate (not . Text.null) name)
