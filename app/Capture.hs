module Capture where

import Content
import Control.Applicative (optional)
import Control.Concurrent (threadDelay)
import Data.Fixed (Micro, showFixed)
import Data.Foldable (for_)
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
import System.Console.ANSI (clearLine, setCursorColumn)
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
    }

parseCaptureArgs :: Parser CaptureArgs
parseCaptureArgs = do
    captureAction <- parseCaptureAction
    delay <-
        optional . option auto $
            long "delay" <> metavar "T" <> help "Delay the capture by T seconds"
    pure CaptureArgs{..}

countDown :: Micro -> IO ()
countDown t = do
    for_ @[] [t, t - dt .. dt] \t' -> do
        clearLine
        putStr $ showFixed True t'
        setCursorColumn 0
        threadDelay . round $ dt * 1_000_000
    clearLine
  where
    dt :: Micro
    dt = 0.01

capture :: CaptureArgs -> Region -> IO Content
capture CaptureArgs{..} region = do
    mapM_ countDown delay
    case captureAction of
        Screenshot -> png <$> Grim.screenshotRegion region
        Video -> mp4 <$> WfRecorder.recordRegion region
