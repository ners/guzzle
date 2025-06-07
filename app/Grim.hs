module Grim where

import Region
import Prelude

grim :: [Text] -> IO LazyByteString
grim p = flip cmd nullStream $ "grim" :| p <> ["-"]

screenshotScreen :: IO LazyByteString
screenshotScreen = grim []

screenshotRegion :: Region -> IO LazyByteString
screenshotRegion region = grim ["-g", ishow region]
