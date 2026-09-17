module WfRecorder where

import Content
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Fixed (Micro)
import Region
import System.IO.Extra (withTempFile)
import System.Process.Typed (Process)
import System.Process.Typed qualified as Process
import Prelude

muxerArgs :: ContentType -> [String]
muxerArgs WEBM = ["--muxer", "webm", "--codec", "libvpx-vp9"]
muxerArgs _ = ["--muxer", "mp4", "--codec", "libx264"]

audioArgs :: Bool -> Maybe String -> [String]
audioArgs _ (Just device) = ["--audio=" <> device]
audioArgs True Nothing = ["--audio"]
audioArgs False Nothing = []

wfRecorder
    :: ContentType
    -> Bool
    -> Maybe String
    -> Maybe Int
    -> Region
    -> FilePath
    -> IO (Process () () ())
wfRecorder format audio audioDevice framerate region file =
    Process.startProcess
        . Process.setStdin nullStream
        . Process.setStdout nullStream
        . Process.setStderr nullStream
        . Process.proc "wf-recorder"
        $ mconcat
            [ ["--geometry", show region]
            , muxerArgs format
            , audioArgs audio audioDevice
            , maybe [] (\r -> ["--framerate", show r]) framerate
            , ["--file", file]
            , ["--overwrite"]
            ]

recordRegion
    :: ContentType
    -> Bool
    -> Maybe String
    -> Maybe Int
    -> Maybe Micro
    -> Region
    -> IO LazyByteString
recordRegion format audio audioDevice framerate (fromMaybe 3 -> duration) region = withTempFile \file -> do
    p <- wfRecorder format audio audioDevice framerate region file
    countdown "Recording: " duration
    Process.stopProcess p >> Process.waitExitCode p >>= \case
        ExitSuccess -> LazyByteString.readFile file
        _ -> fatalError "wf-recorder failed"
