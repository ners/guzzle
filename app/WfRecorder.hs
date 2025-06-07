module WfRecorder where

import Region
import Prelude

wfRecorder :: [Text] -> IO LazyByteString
wfRecorder p = flip cmd "" $ "wf-recorder" :| p <> ["-o", "-"]

recordRegion :: Region -> IO LazyByteString
recordRegion region = wfRecorder ["-g", ishow region]
