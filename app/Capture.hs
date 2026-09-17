module Capture where

import Content
import Data.Fixed (Micro)
import Grim qualified
import Region
import WfRecorder qualified
import Prelude

data CaptureAction
    = Screenshot
    | Video
    deriving stock (Eq)

data CaptureArgs = CaptureArgs
    { captureAction :: CaptureAction
    , delay :: Maybe Micro
    , duration :: Maybe Micro
    }

capture :: CaptureArgs -> Region -> IO Content
capture CaptureArgs{..} region = do
    mapM_ (countdown "Starting in: ") delay
    case captureAction of
        Screenshot -> png <$> Grim.screenshotRegion region
        Video -> mp4 <$> WfRecorder.recordRegion duration region
