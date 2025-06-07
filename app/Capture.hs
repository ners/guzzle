module Capture where

import Content
import Grim qualified
import Options.Applicative
    ( Alternative ((<|>))
    , Parser
    , argument
    , help
    , maybeReader
    , metavar
    )
import Region
import WfRecorder qualified
import Prelude

data CaptureAction
    = Screenshot
    | Video
    deriving stock (Eq)

parseCaptureAction :: Parser CaptureAction
parseCaptureAction =
    foldr1 @[]
        (<|>)
        [ captureArgument
            Screenshot
            "screenshot"
            "Make a screenshot of the selected region"
        , captureArgument Video "video" "Record a video of the selected region"
        , pure Screenshot
        ]
  where
    captureArgument
        :: CaptureAction
        -> String
        -> String
        -> Parser CaptureAction
    captureArgument s str helpStr =
        flip argument (metavar str <> help helpStr) . maybeReader $ \str' ->
            if str' == str then Just s else Nothing

newtype CaptureArgs = CaptureArgs
    { captureAction :: CaptureAction
    }

parseCaptureArgs :: Parser CaptureArgs
parseCaptureArgs = do
    captureAction <- parseCaptureAction
    pure CaptureArgs{..}

capture :: CaptureArgs -> Region -> IO Content
capture CaptureArgs{captureAction = Screenshot} = fmap png . Grim.screenshotRegion
capture CaptureArgs{captureAction = Video} = fmap mp4 . WfRecorder.recordRegion
