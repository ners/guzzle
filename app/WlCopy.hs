module WlCopy where

import System.Process.Typed qualified as Process
import Prelude

wlCopy :: LazyByteString -> IO ()
wlCopy = cmd_ ["wl-copy"] . Process.byteStringInput
