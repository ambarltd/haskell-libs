-- | The contents and types of Ambar records.
module Ambar.Record where

import Control.Applicative (asum)
import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON)
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Base64.Types (extractBase64)
import Data.Binary (Binary)
import qualified Data.Binary as Binary
import Data.Binary.Instances.Aeson ()
import Data.ByteString (ByteString)
import Data.ByteString.Base64 (decodeBase64Untyped, encodeBase64)
import Data.Char (toUpper)
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Prettyprinter

-- | A piece of data that is retrieved from a data source,
-- stored in a queue and projected onto a data destination.
newtype Record = Record
  { content :: Content
  }
  deriving (Show, Generic, Eq)

instance Binary Record

type Content = [(Text, Value)]

-- | A schema is an ordered list of key-value pairs.
-- Every field is always optional.
-- Backward-compatible updates are the ones that only ad new key-value pairs.
newtype Schema = Schema [Field]
  deriving newtype (ToJSON, FromJSON, Semigroup, Monoid)

data Field = Field
  { field_name :: Text
  , field_type :: Type
  }
  deriving (Generic, Show)

instance FromJSON Field
instance ToJSON Field

-- | The rich set of types that we support.
data Type
  -- base types
  = TBoolean
  | TUInteger
  | TInteger
  | TReal
  | TString
  | TBytes
  | TJSON
  -- rich types
  | TDateTime
  deriving (Generic, Show, Eq, Enum, Bounded, Ord)

-- | Case insensitive parsing.
instance FromJSON Type where
  parseJSON = Json.withText "Type" $ \t ->
    case lookup (uppercase t) mapping of
      Just ty -> return ty
      Nothing -> Json.parseFail $ "Unknown type '" <> Text.unpack t <> "'"
    where
    mapping :: [(Text, Type)]
    mapping =
      [ (name, ty)
      | ty <- [minBound..] :: [Type]
      , let name = removeT $ uppercase $ Text.pack $ show ty
      ]
    uppercase = Text.toUpper
    removeT s = fromMaybe s (Text.stripPrefix "T" s)

instance ToJSON Type
  where toJSON = Json.genericToJSON optionsType

optionsType :: Json.Options
optionsType = Json.defaultOptions
  { Json.constructorTagModifier = \label ->
      fromMaybe label (stripPrefix "T" label)
  }

-- | One constructor for each inhabitant of Type
data Value
  = Boolean Bool
  | UInt Word64
  | Int Int64
  | Real Double
  | String Text
  | Binary Bytes
  | Json Json.Value
  | DateTime TimeStamp
    -- ^ Only dates in format ISO8601 are supported.
  | Null
  deriving (Show, Generic, Eq, Hashable)

-- | A timestamp value with the original timestamp string value.
-- We have a Maybe UTCTime here because we want to be lenient to
-- invalid dates being ingested.
data TimeStamp = TimeStamp Text (Maybe UTCTime)
  deriving (Eq, Hashable)
  deriving stock (Show, Generic)

toTimeStamp :: Text -> TimeStamp
toTimeStamp txt =
  TimeStamp txt mdate
  where
  str = Text.unpack txt
  mdate = asum
    [ iso8601ParseM str
    -- We support UTC dates without the `Z` at the end
    -- because of an error in the MySQL connector which
    -- was generating dates of this kind.
    , iso8601ParseM (str <> "Z")
    ]

instance Binary TimeStamp where
  put (TimeStamp txt _) = Binary.put txt
  get = toTimeStamp <$> Binary.get

instance ToJSON TimeStamp where
  toJSON (TimeStamp txt _) = toJSON txt

instance FromJSON TimeStamp where
  parseJSON = Json.withText "TimeStamp" (return . toTimeStamp)

instance Binary Value

instance FromJSON Value where
  parseJSON = Json.withObject "Value" $ \val ->
    case KeyMap.toList val of
      [(k,v)] ->
        case k of
          "BOOLEAN" -> Boolean <$> parseJSON v
          "UINT"    -> UInt <$> parseJSON v
          "INT"     -> Int <$> parseJSON v
          "REAL"    -> Real <$> parseJSON v
          "STRING"  -> String <$> parseJSON v
          "BINARY"  -> Binary <$> parseJSON v
          -- invalid JSON are treated as String.
          "JSON" -> return $ Json v
          -- invalid DateTimes are treated as String. Only ISO8601 dates are supported.
          "DATETIME" -> DateTime . toTimeStamp <$> parseJSON v
          "NULL" -> return Null
          _ -> fail $ "invalid Value type: " <> show k
      _ -> fail "Expected exactly one key"

instance ToJSON Value
  where toJSON = Json.genericToJSON options

options :: Json.Options
options = Json.defaultOptions
  { Json.sumEncoding = Json.ObjectWithSingleField
  , Json.constructorTagModifier = map toUpper
  }

newtype Bytes = Bytes ByteString
  deriving newtype (Binary, Eq, Hashable)

toBase64 :: Bytes -> Text
toBase64 (Bytes bs) = extractBase64 $ encodeBase64 bs

instance Show Bytes where
  show bytes = "Bytes " <> show (pretty bytes)

instance Pretty Bytes where
  pretty bytes = pretty $ show $ Text.unpack $ toBase64 bytes

instance ToJSON Bytes where
  toJSON bytes = Json.String (toBase64 bytes)

instance FromJSON Bytes where
  parseJSON = Json.withText "Bytes" $ \txt ->
    either fail return (fromBase64 txt)
    where
    fromBase64 txt =
      case decodeBase64Untyped (Text.encodeUtf8 txt) of
        Left err -> Left $ Text.unpack err
        Right v -> Right $ Bytes v
