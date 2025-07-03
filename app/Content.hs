module Content where

import Prelude

data ContentType = PNG | MP4
    deriving stock (Eq)

data Content = Content
    { content :: LazyByteString
    , contentType :: ContentType
    }

png :: LazyByteString -> Content
png content = Content{contentType = PNG, ..}

mp4 :: LazyByteString -> Content
mp4 content = Content{contentType = MP4, ..}

extension :: ContentType -> String
extension PNG = "png"
extension MP4 = "mp4"

mimetype :: ContentType -> Text
mimetype MP4 = "video/mp4"
mimetype PNG = "image/png"
