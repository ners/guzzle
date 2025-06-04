module Main where

import Capture
import Hyprctl qualified
import Selection
import Sink
import Slurp qualified
import Swaymsg qualified
import Prelude

data Args = Args
    { selectionArgs :: SelectionArgs
    , captureArgs :: CaptureArgs
    , sinkArgs :: SinkArgs
    }

main :: IO ()
main = do
    let Args{..} =
            Args
                { selectionArgs = SelectionArgs{selectionMode = Output}
                , captureArgs = CaptureArgs{}
                , sinkArgs = SinkArgs{}
                }
    selection selectionArgs >>= capture captureArgs >>= sink sinkArgs
