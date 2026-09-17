module Sink where

import Content
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601
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
        maybe (("guzzle-" <>) . iso8601Show <$> getCurrentTime) pure file
            <&> (-<.> extension contentType)
    let hasFile = isJust file || sinkAction == Just Save || isVideo contentType
    when hasFile $ LazyByteString.writeFile filename content
    case fromMaybe Copy sinkAction of
        Copy | hasFile -> WlCopy.wlCopyFile filename
        Copy -> WlCopy.wlCopy Content{..}
        Save -> pure ()
        Print -> LazyByteString.putStr content
