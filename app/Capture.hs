module Capture where

import Selection
import Prelude

data CaptureArgs = CaptureArgs
    {}

capture :: CaptureArgs -> Selection -> IO ByteString
capture _ _ = pure ""
