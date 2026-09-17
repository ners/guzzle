module Grim where

import Content
import Control.Monad (guard)
import Region
import Prelude

grim :: [Text] -> IO LazyByteString
grim p = flip cmd nullStream $ "grim" :| p <> ["-"]

formatArg :: ContentType -> [Text]
formatArg JPEG = ["-t", "jpeg"]
formatArg PPM = ["-t", "ppm"]
formatArg _ = []

screenshotRegion
    :: ContentType
    -> Bool
    -> Maybe Int
    -> Maybe Double
    -> Region
    -> IO LazyByteString
screenshotRegion format cursor quality scale region =
    grim $
        ["-g", ishow region]
            <> formatArg format
            <> ["-c" | cursor]
            <> maybe [] (\q -> ["-q", ishow q]) (quality <* guard (format == JPEG))
            <> maybe [] (\s -> ["-s", ishow s]) scale
