module Capture where

import Content
import Control.Applicative (optional)
import Data.Fixed (Micro)
import Grim qualified
import Options.Applicative
    ( Alternative ((<|>))
    , Parser
    , argument
    , auto
    , help
    , long
    , maybeReader
    , metavar
    , option
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

data CaptureArgs = CaptureArgs
    { captureAction :: CaptureAction
    , delay :: Maybe Micro
    , duration :: Maybe Micro
    }

parseCaptureArgs :: Parser CaptureArgs
parseCaptureArgs = do
    captureAction <- parseCaptureAction
    delay <-
        optional . option auto $
            long "delay" <> metavar "T" <> help "Delay the capture by T seconds"
    duration <-
        optional . option auto $
            long "duration" <> metavar "T" <> help "Record video for T seconds"
    pure CaptureArgs{..}

capture :: CaptureArgs -> Region -> IO Content
capture CaptureArgs{..} region = do
    mapM_ (countdown "Starting in: ") delay
    case captureAction of
        Screenshot -> png <$> Grim.screenshotRegion region
        Video -> mp4 <$> WfRecorder.recordRegion duration region
