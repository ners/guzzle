module WlCopy where

import Content
import Data.ByteString qualified as ByteString
import Data.Char (chr, isAsciiLower, isAsciiUpper, isDigit)
import Data.Foldable (toList)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Traversable (for)
import Data.Word (Word8)
import Numeric (showHex)
import System.Directory (canonicalizePath)
import System.Process.Typed qualified as Process
import Prelude

wlCopy :: Content -> IO ()
wlCopy Content{..} =
    cmd_ ["wl-copy", "--type", mimetype contentType] $
        Process.byteStringInput content

wlCopyFiles :: NonEmpty FilePath -> IO ()
wlCopyFiles files = do
    uris <- for files \file -> ("file:" <>) . percentEncode <$> canonicalizePath file
    cmd_ ["wl-copy", "--type", "text/uri-list"] . textInput $
        Text.intercalate "\r\n" (toList uris)

percentEncode :: FilePath -> Text
percentEncode =
    foldMap percentEncodeByte . ByteString.unpack . Text.encodeUtf8 . Text.pack

percentEncodeByte :: Word8 -> Text
percentEncodeByte byte
    | isUnreserved char = Text.singleton char
    | otherwise =
        "%" <> Text.toUpper (Text.justifyRight 2 '0' (Text.pack (showHex byte "")))
  where
    char = chr $ fromIntegral byte

isUnreserved :: Char -> Bool
isUnreserved char =
    isAsciiUpper char
        || isAsciiLower char
        || isDigit char
        || elem @[] char "-._~/"
