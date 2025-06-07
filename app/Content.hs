module Content where

import Prelude

data ContentType = PNG | MP4

data Content = Content
    { content :: LazyByteString
    , contentType :: ContentType
    }

png :: LazyByteString -> Content
png content = Content{contentType = PNG, ..}

mp4 :: LazyByteString -> Content
mp4 content = Content{contentType = MP4, ..}

filename :: ContentType -> String -> String
filename PNG = (<> ".png")
filename MP4 = (<> ".mp4")
