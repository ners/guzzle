module Sink where

import Content
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text qualified as Text
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601
import Notify qualified
import System.Directory (canonicalizePath)
import System.FilePath ((-<.>))
import WlCopy qualified
import Prelude

data SinkAction
    = Copy
    | Save
    | Print
    deriving stock (Eq)

data SinkArgs = SinkArgs
    { sinkAction :: Maybe SinkAction
    , file :: Maybe FilePath
    }

sink :: SinkArgs -> Content -> IO ()
sink SinkArgs{..} Content{..} = do
    filename <-
        canonicalizePath
            =<< ( maybe (("guzzle-" <>) . iso8601Show <$> getCurrentTime) pure file
                    <&> (-<.> extension contentType)
                )
    let hasFile = isJust file || sinkAction == Just Save || isVideo contentType
        kind = if isVideo contentType then "Video" else "Screenshot"
    when hasFile do
        LazyByteString.writeFile filename content
        printInfo $ "Saved file " <> Text.pack filename
    case fromMaybe Copy sinkAction of
        Copy | hasFile -> do
            WlCopy.wlCopyFile filename
            Notify.notify (kind <> " saved and copied") (Text.pack filename)
        Copy -> do
            WlCopy.wlCopy Content{..}
            Notify.notify (kind <> " copied to clipboard") ""
        Save -> Notify.notify (kind <> " saved") (Text.pack filename)
        Print -> LazyByteString.putStr content
