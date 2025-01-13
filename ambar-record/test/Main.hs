{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import qualified Data.Aeson as Aeson
import Data.Base64.Types (extractBase64)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Base64 (encodeBase64)
import qualified Data.Map.Strict as Map
import Data.Time.Format.ISO8601 (iso8601Show, iso8601ParseM)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import GHC.IsList (fromList)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Test.Hspec (Spec, hspec, describe, it, shouldBe)
import Test.QuickCheck (Arbitrary(..), Gen, property, arbitrary)
import qualified Test.QuickCheck as Q

import Ambar.Record (Record(..), Value(..), Bytes(..), TimeStamp(..))
import Ambar.Record.Encoding (Encode, Decode, encode, decode, TaggedJson)
import Ambar.Record.Encoding.TaggedBinary (TaggedBinary(..))

main :: IO ()
main = hspec testEncodings

testEncodings :: Spec
testEncodings = describe "encoding" $ do
  describe "TaggedJson" $ do
    it "model-checking" $ run @TaggedJson Proxy

  describe "TaggedBinary" $ do
    it "model-checking" $ run @TaggedBinary Proxy
    describe "golden test" $ do
      let test :: [(Text, Value)] -> Text -> IO ()
          test fields result = do
            let TaggedBinary encoded = encode (Record fields)
            extractBase64 (encodeBase64 (BS.toStrict encoded)) `shouldBe` result
      it "Boolean" $ test [("field_Boolean", Boolean False) ] "AQAAAAAAAAAXAAAAAAAAAA1maWVsZF9Cb29sZWFuAAA="
      it "UInteger" $ test [("field_UInteger", UInt 1)] "AQAAAAAAAAAfAAAAAAAAAA5maWVsZF9VSW50ZWdlcgEAAAAAAAAAAQ=="
      it "Integer" $ test [("field_Integer", Int (-1))] "AQAAAAAAAAAeAAAAAAAAAA1maWVsZF9JbnRlZ2VyAv//////////"
      it "Real" $ test [("field_Real", Real 1.5)] "AQAAAAAAAAAbAAAAAAAAAApmaWVsZF9SZWFsAz/4AAAAAAAA"
      it "String" $ test [("field_String", String "wat")] "AQAAAAAAAAAgAAAAAAAAAAxmaWVsZF9TdHJpbmcEAAAAAAAAAAN3YXQ="
      it "Bytes" $ test [("field_Bytes", Binary (Bytes "abc"))] "AQAAAAAAAAAfAAAAAAAAAAtmaWVsZF9CeXRlcwUAAAAAAAAAA2FiYw=="
      it "JSON" $ test
        [("field_JSON", jsonValue $ Aeson.Object $ fromList
          [ ("null", Aeson.Null)
          , ("string", Aeson.String "some_string")
          , ("number", Aeson.Number 9)
          , ("array", Aeson.Array $ fromList [Aeson.Number 1])
          , ("bool", Aeson.Bool True)
          , ("object" , Aeson.Object $ fromList [("first", Aeson.Bool True)])
          ]
        )] "AQAAAAAAAAB6AAAAAAAAAApmaWVsZF9KU09OBwAAAAAAAABfeyJhcnJheSI6WzFdLCJib29sIjp0cnVlLCJudWxsIjpudWxsLCJudW1iZXIiOjksIm9iamVjdCI6eyJmaXJzdCI6dHJ1ZX0sInN0cmluZyI6InNvbWVfc3RyaW5nIn0="
      it "DateTime" $ do
        let txt = "2024-01-23T00:26:17Z"
        time <- iso8601ParseM txt
        test [("field_DateTime", DateTime (TimeStamp (Text.pack txt) time))] "AQAAAAAAAAAzAAAAAAAAAA5maWVsZF9EYXRlVGltZQYAAAAAAAAAFDIwMjQtMDEtMjNUMDA6MjY6MTda"
      it "all" $  do
        let txt = "2024-01-23T00:26:17Z"
        time <- iso8601ParseM txt
        let datetime = DateTime (TimeStamp (Text.pack txt) time)
        let record = Record
              [ ("field_Boolean", Boolean False)
              , ("field_UInteger", UInt 1)
              , ("field_Integer", Int (-1))
              , ("field_Real", Real 1.5)
              , ("field_String", String "wat")
              , ("field_Bytes", Binary (Bytes "abc"))
              , ("field_JSON", jsonValue $ Aeson.Object $ fromList
                  [ ("null", Aeson.Null)
                  , ("string", Aeson.String "some_string")
                  , ("number", Aeson.Number 9)
                  , ("array", Aeson.Array $ fromList [Aeson.Number 1])
                  , ("bool", Aeson.Bool True)
                  , ("object" , Aeson.Object $ fromList [("first", Aeson.Bool True)])
                  ]
                )
              , ("field_DateTime", datetime)
              ]
        let TaggedBinary encoded = encode record
            expected = "AQAAAAAAAAFbAAAAAAAAAA1maWVsZF9Cb29sZWFuAAAAAAAAAAAADmZpZWxkX1VJbnRlZ2VyAQAAAAAAAAABAAAAAAAAAA1maWVsZF9JbnRlZ2VyAv//////////AAAAAAAAAApmaWVsZF9SZWFsAz/4AAAAAAAAAAAAAAAAAAxmaWVsZF9TdHJpbmcEAAAAAAAAAAN3YXQAAAAAAAAAC2ZpZWxkX0J5dGVzBQAAAAAAAAADYWJjAAAAAAAAAApmaWVsZF9KU09OBwAAAAAAAABfeyJhcnJheSI6WzFdLCJib29sIjp0cnVlLCJudWxsIjpudWxsLCJudW1iZXIiOjksIm9iamVjdCI6eyJmaXJzdCI6dHJ1ZX0sInN0cmluZyI6InNvbWVfc3RyaW5nIn0AAAAAAAAADmZpZWxkX0RhdGVUaW1lBgAAAAAAAAAUMjAyNC0wMS0yM1QwMDoyNjoxN1o="
        extractBase64 (encodeBase64 (BS.toStrict encoded)) `shouldBe` expected
  where
    run :: forall a. (Encode a, Decode a) => Proxy a -> Q.Property
    run _ = Q.withMaxSuccess 1000 $ property $ \record -> do
      let schema = error "decoding using schemas not implemented yet"
      decode schema (encode record :: a) `shouldBe` Right record

instance Arbitrary Record where
  arbitrary = Record . sorted <$> pairs
    where
    sorted = Map.toList . Map.fromList

    pairs :: Gen [(Text, Value)]
    pairs = Q.resize 10 (Q.listOf pair)

    pair :: Gen (Text, Value)
    pair = do
      FieldName key <- arbitrary
      value <- arbitrary
      return (key, value)

  shrink (Record content) =
    case content of
      [] -> []
      -- if it has one field, shrink the key and value
      [(k, v)] -> Record <$>
        [ pure (key, value)
        | FieldName key <- shrink (FieldName k)
        , value <- shrink v
        ]
      -- if it has multiple fields, create records with a single field
      pairs -> Record <$>
        [ pure (key, value)
        | (key, value) <- pairs
        ]

instance Arbitrary TimeStamp where
  arbitrary = do
    time <- posixSecondsToUTCTime . fromInteger <$> arbitrary
    let txt = Text.pack $ iso8601Show time
    return $ TimeStamp txt time

  shrink (TimeStamp _ time) = do
    n <- shrink $ ceiling $ utcTimeToPOSIXSeconds time
    let time' = posixSecondsToUTCTime $ fromInteger n
        txt = Text.pack $ iso8601Show time'
    return $ TimeStamp txt time'

instance Arbitrary Value where
  arbitrary = Q.oneof
    [ Boolean <$> Q.arbitraryBoundedEnum
    , Int <$> arbitrary
    , UInt <$> arbitrary
    , Real <$> arbitrary
    , do
        FieldName n <- arbitrary
        return $ String n
      -- binary of up to 1Kb
    , Binary . Bytes . BS.pack <$> Q.resize 1024 (Q.listOf arbitrary)
    , DateTime <$> arbitrary
    , return $ jsonValue Aeson.Null
    , return Null
    ]

  shrink = \case
    Boolean _ -> []
    Int n -> Int <$> shrink n
    UInt n -> UInt <$> shrink n
    Real n -> Real <$> shrink n
    String txt -> String . unFieldName <$> shrink (FieldName txt)
    Binary (Bytes bs) -> Binary . Bytes <$> BS.tails bs
    DateTime timestamp -> DateTime <$> shrink timestamp
    Json _ json -> jsonValue <$> shrink json
    Null -> []

jsonValue :: Aeson.Value -> Value
jsonValue value = Json (Text.decodeUtf8 $ LB.toStrict $ Aeson.encode value) value

-- A text of an appropriate length to be used as the name of a field
newtype FieldName = FieldName { unFieldName :: Text }

instance Arbitrary FieldName where
  -- A piece of text of pu to 20 characters.
  arbitrary = FieldName . Text.pack <$> Q.resize 20 (Q.listOf arbitrary)

  shrink (FieldName txt) = FieldName <$> reverse (Text.tails txt)
