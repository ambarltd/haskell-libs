module Ambar.Record.Encoding.TaggedBinary (TaggedBinary(..)) where

import qualified Data.Aeson as Aeson
import Data.Binary.Builder (Builder)
import qualified Data.Binary.Builder as Builder
import qualified Data.Binary.Get as Get
import Data.Binary.Get (Get)
import qualified Data.Binary.Put as Put
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Encoding as Text
import Control.Monad (unless)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import Ambar.Record (Record(..), Value(..), Bytes(..))
import Ambar.Record.Encoding (Encode(..), Decode(..))

newtype TaggedBinary = TaggedBinary ByteString

instance Encode TaggedBinary where
  encode (Record fields) = TaggedBinary $ Builder.toLazyByteString encoded
    where
    encoded = versionTag <> withLength pairs

    withLength builder = length' <> content
      where
        bs = Builder.toLazyByteString builder
        length' = Builder.putWord64be (fromIntegral $ LB.length bs)
        content = Builder.fromLazyByteString bs

    versionTag = Builder.singleton 1

    pairs = foldMap pair fields

    pair (k, v) = string k <> value v

    string k = withLength str
      where str = Builder.fromLazyByteString $ LB.fromStrict $ encodeUtf8 k

    value v = tag v <> content v
      where
      content = \case
        Boolean True  -> Builder.singleton 0xFF
        Boolean False -> Builder.singleton 0x00
        Int n -> Put.execPut $ Put.putInt64be n
        UInt n -> Builder.putWord64be n
        Real n -> Put.execPut $ Put.putDoublebe n
        String t -> string t
        Binary (Bytes b) -> withLength (Builder.fromByteString b)
        Json txt _ -> withLength
          $ Builder.fromLazyByteString
          $ LB.fromStrict
          $ Text.encodeUtf8 txt
        DateTime txt -> string txt
        Null -> mempty

      tag :: Value -> Builder
      tag = Builder.singleton . \case
        Boolean _ -> 0
        UInt _ -> 1
        Int _ -> 2
        Real _ -> 3
        String _ -> 4
        Binary _ -> 5
        DateTime _ -> 6
        Json _ _ -> 7
        Null -> 8

instance Decode TaggedBinary where
  decode _ (TaggedBinary raw) =
    case Get.runGetOrFail record raw of
      Left (_,_,err) -> Left $ "unable to decode binary record: " <> pack err
      Right (_,_,r) -> Right r
    where
    record = do
      checkVersion
      pairs <- manyWithLength pair
      checkEnd
      return $ Record pairs

    checkVersion = do
      v <- Get.getWord8
      unless (v == 1) $ fail $
        "Incorrect version. Expected 0 but got " <> show v

    checkEnd = do
      finished <- Get.isEmpty
      unless finished $ do
        here <- Get.bytesRead
        let total = LB.length raw
            remaining = total - here
        fail $ "extra " <> show remaining <> " bytes at the end of value"

    manyWithLength decodeOne = do
      total <- Get.getWord64be
      start <- Get.bytesRead
      let consume xs = do
            here <- Get.bytesRead
            let consumed = fromIntegral $ here - start
                done = consumed >= total
            if done
               then return (reverse xs)
               else do
                 x <- decodeOne
                 consume (x:xs)
      consume []

    pair :: Get.Get (Text, Value)
    pair = do
      key <- string
      val <- value
      return (key, val)

    value = do
      tag <- Get.getWord8
      case tag of
        0 -> boolean
        1 -> uint
        2 -> int
        3 -> real
        4 -> String <$> string
        5 -> Binary . Bytes <$> bytestring
        6 -> datetime
        7 -> json
        8 -> return Null
        _ -> fail $ "unknown tag: " <> show tag

    boolean = do
      val <- Get.getWord8
      case val of
        0x00 -> return $ Boolean False
        0xFF -> return $ Boolean True
        _ -> fail $ "unknown boolean code: " <> show val

    uint = UInt . fromIntegral <$> Get.getWord64be

    int = Int . fromIntegral <$> Get.getInt64be

    real = Real <$> Get.getDoublebe

    datetime = DateTime <$> string

    json = do
      bs <- bytestring
      case Aeson.eitherDecode (LB.fromStrict bs) of
        Left err -> fail $ "unable to decode json field: " <> show err
        Right val -> return $ Json (Text.decodeUtf8 bs) val

    string :: Get Text
    string = decodeUtf8 <$> bytestring

    bytestring :: Get BS.ByteString
    bytestring = do
      len <- Get.getWord64be
      Get.getByteString (fromIntegral len)
