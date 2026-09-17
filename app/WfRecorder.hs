module WfRecorder where

import Content
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Fixed (Micro)
import Data.Foldable (for_)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Traversable (for)
import Region (Region (..))
import System.FilePath ((<.>), (</>))
import System.IO.Extra (withTempDir)
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

scaleFilter :: Double -> String
scaleFilter factor =
    "scale=trunc(iw*" <> show factor <> "/2)*2:trunc(ih*" <> show factor <> "/2)*2"

wfRecorder
    :: ContentType
    -> Bool
    -> Maybe String
    -> Maybe Int
    -> Maybe Double
    -> Region
    -> FilePath
    -> IO (Process () () ())
wfRecorder format audio audioDevice framerate scale region file =
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
            , maybe [] (\s -> ["--filter", scaleFilter s]) scale
            , ["--file", file]
            , ["--overwrite"]
            ]

recordRegions
    :: ContentType
    -> Bool
    -> Maybe String
    -> Maybe Int
    -> Maybe Double
    -> Maybe Micro
    -> NonEmpty Region
    -> IO (NonEmpty LazyByteString)
recordRegions format audio audioDevice framerate scale (fromMaybe 3 -> duration) regions = withTempDir \dir -> do
    let files =
            NonEmpty.zipWith
                (\i _ -> dir </> show @Int i <.> extension format)
                (1 :| [2 ..])
                regions
    processes <-
        for (NonEmpty.zip regions files) \(region, file) ->
            wfRecorder format audio audioDevice framerate scale region file
    countdown "Recording: " duration
    for_ processes Process.stopProcess
    for (NonEmpty.zip processes files) \(process, file) ->
        Process.waitExitCode process >>= \case
            ExitSuccess -> LazyByteString.readFile file
            _ -> fatalError "wf-recorder failed"
