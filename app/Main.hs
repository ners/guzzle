module Main where

import Capture
import Selection
import Sink
import Prelude
import Hyprctl qualified

data Args = Args
    { selectionArgs :: SelectionArgs
    , captureArgs :: CaptureArgs
    , sinkArgs :: SinkArgs
    }

main :: IO ()
main = do
    mapM_ print =<< Hyprctl.getVisibleWindows
    let Args{..} =
            Args
                { selectionArgs = SelectionArgs{}
                , captureArgs = CaptureArgs{}
                , sinkArgs = SinkArgs{}
                }
    selection selectionArgs >>= capture captureArgs >>= sink sinkArgs
