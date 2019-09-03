module Optima
(
  -- * IO
  params,
  -- * Params
  Params,
  param,
  group,
  -- * ParamGroup
  ParamGroup,
  member,
  subgroup,
  -- * Param
  Param,
  value,
  flag,
  -- * Value
  Value,
  explicitlyParsed,
  implicitlyParsed,
  -- * Default
  Default,
  explicitlyRepresented,
  showable,
  defaultless,
  -- * ValueFormat
  ValueFormat,
  formattedByEnum,
  formattedByEnumUsingShow,
  unformatted,
)
where

import Optima.Prelude hiding (group)
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
Parameter group, which gets identified by prefixing the names.

Should be used to define parameters, which only make sense in combination.
E.g., a server config can be defined by providing port and host together.
-}
newtype ParamGroup a = ParamGroup (Text -> Optparse.Parser a)

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

deriving instance Functor ParamGroup
instance Applicative ParamGroup where
  pure x = ParamGroup (\ _ -> pure x)
  (<*>) (ParamGroup left) (ParamGroup right) = ParamGroup (\ prefix -> left prefix <*> right prefix)
instance Alternative ParamGroup where
  empty = ParamGroup (\ _ -> empty)
  (<|>) (ParamGroup left) (ParamGroup right) = ParamGroup (\ prefix -> left prefix <|> right prefix)
  many (ParamGroup parser) = ParamGroup (\ prefix -> many (parser prefix))
  some (ParamGroup parser) = ParamGroup (\ prefix -> some (parser prefix))

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


-- ** Params
-------------------------

{-|
Lift a single parameter parser.
-}
param :: Maybe Char {-^ Single-char name -} -> Text {-^ Long name -} -> Param a -> Params a
param shortName longName (Param parser) = Params (parser shortName longName)

{-|
Lift a parameter group parser.

The param group cannot use short names, only long names.
-}
group :: Text {-^ Prefix for the long names of the parameters. If empty, then there'll be no prefixing -} -> ParamGroup a -> Params a
group prefix (ParamGroup parser) = Params (parser prefix)


-- ** ParamGroup
-------------------------

{-|
Lift a param parser into parameter group.
-}
member :: Text {-^ Long name of the parameter -} -> Param a -> ParamGroup a
member name (Param parser) = ParamGroup (\ prefix -> parser Nothing (prefixIfMakesSense prefix name)) where

{-|
Unite a group by a shared prefix.
-}
subgroup :: Text {-^ Long name prefix -} -> ParamGroup a -> ParamGroup a
subgroup prefix (ParamGroup parser) = ParamGroup (\ higherPrefix -> parser (prefixIfMakesSense higherPrefix prefix))


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

{-|
A parameter with no value. Fails if it's not present.
Thus it can be composed using Alternative.
-}
flag :: Text {-^ Description. Can be empty -} -> Param ()
flag description =
  Param (\ shortName longName ->
    Optparse.flag' ()
      (longParamName longName <> foldMap Optparse.short shortName <> paramHelp description UnspecifiedFormat))


-- ** Value
-------------------------

{-|
Lift an Attoparsec parser into value parser.
-}
explicitlyParsed :: Attoparsec.Parser a -> Value a
explicitlyParsed = Value

{-|
Lift an implicit lenient Attoparsec parser into value parser.
-}
implicitlyParsed :: Attoparsec.LenientParser a => Value a
implicitlyParsed = Value Attoparsec.lenientParser


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
Derive value format specification from the Enum instance and
explicit mapping of values to their representations.
-}
formattedByEnum :: (Bounded a, Enum a) => (a -> Text) -> ValueFormat a
formattedByEnum valueRepresentation = formattedByEnumUsingBuilderMapping (TextBuilder.text . valueRepresentation)

{-|
Derive value format specification from the Enum and Show instances.
-}
formattedByEnumUsingShow :: (Bounded a, Enum a, Show a) => ValueFormat a
formattedByEnumUsingShow = formattedByEnumUsingBuilderMapping (TextBuilder.string . show)

{-|
Derive value format specification from the Enum instance and
explicit mapping of values to their representations.
-}
formattedByEnumUsingBuilderMapping :: (Bounded a, Enum a) => (a -> TextBuilder.Builder) -> ValueFormat a
formattedByEnumUsingBuilderMapping valueRepresentation = let
  values = enumFromTo minBound (asTypeOf maxBound (descriptionToA description))
  descriptionToA = undefined :: ValueFormat a -> a
  description = EnumValueFormat (fmap valueRepresentation values)
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

prefixIfMakesSense :: Text -> Text -> Text
prefixIfMakesSense prefix text = if Text.null prefix
  then text
  else prefix <> "-" <> text


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
