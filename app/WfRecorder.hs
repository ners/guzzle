module WfRecorder where

import Data.ByteString.Lazy qualified as LazyByteString
import Data.Fixed (Micro)
import Region
import System.IO.Extra (withTempFile)
import System.Process.Typed (Process)
import System.Process.Typed qualified as Process
import Prelude

wfRecorder :: Region -> FilePath -> IO (Process () () ())
wfRecorder region file =
    Process.startProcess
        . Process.setStdin nullStream
        . Process.setStdout nullStream
        . Process.setStderr nullStream
        . Process.proc "wf-recorder"
        $ mconcat
            [ ["--geometry", show region]
            , ["--muxer", "mp4"]
            , ["--codec", "libx264"]
            , ["--file", file]
            , ["--overwrite"]
            ]

recordRegion :: Maybe Micro -> Region -> IO LazyByteString
recordRegion (fromMaybe 3 -> duration) region = withTempFile \file -> do
    p <- wfRecorder region file
    countdown "Recording: " duration
    Process.stopProcess p >> Process.waitExitCode p >>= \case
        ExitSuccess -> LazyByteString.readFile file
        _ -> fatalError "wf-recorder failed"
