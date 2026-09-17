module Content where

import Data.Char (toLower)
import Data.List qualified as List
import Prelude

data ContentType = PNG | JPEG | PPM | MP4 | WEBM
    deriving stock (Eq, Bounded, Enum)

data Content = Content
    { content :: LazyByteString
    , contentType :: ContentType
    }

isVideo :: ContentType -> Bool
isVideo MP4 = True
isVideo WEBM = True
isVideo _ = False

extension :: ContentType -> String
extension PNG = "png"
extension JPEG = "jpg"
extension PPM = "ppm"
extension MP4 = "mp4"
extension WEBM = "webm"

formatFromExtension :: String -> Maybe ContentType
formatFromExtension ext =
    List.find @[]
        ((==) (toLower <$> dropWhile (== '.') ext) . extension)
        [minBound .. maxBound]

mimetype :: ContentType -> Text
mimetype PNG = "image/png"
mimetype JPEG = "image/jpeg"
mimetype PPM = "image/x-portable-pixmap"
mimetype MP4 = "video/mp4"
mimetype WEBM = "video/webm"
