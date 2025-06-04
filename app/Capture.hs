module Capture where

import Region
import Prelude

data CaptureArgs = CaptureArgs
    {}

capture :: CaptureArgs -> Region -> IO ByteString
capture _ _ = pure ""
