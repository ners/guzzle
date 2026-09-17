module Args where

import Capture
import Content (formatFromExtension)
import Data.Version (showVersion)
import Options.Applicative
import Options.Applicative.Types (Backtracking (..))
import Paths_guzzle qualified
import Selection
import Sink
import Prelude

literal :: a -> String -> String -> Parser a
literal result token helpStr =
    flip argument (metavar token <> help helpStr <> completeWith [token])
        . maybeReader
        $ \arg -> if arg == token then Just result else Nothing

parseSinkAction :: Parser SinkAction
parseSinkAction =
    foldr1 @[]
        (<|>)
        [ literal Copy "copy" "Copy contents to clipboard"
        , literal Save "save" "Save contents to file"
        , literal Print "print" "Print contents to stdout"
        ]

parseSinkArgs :: Parser SinkArgs
parseSinkArgs = do
    sinkAction <- optional parseSinkAction
    file <-
        optional . strOption $
            short 'f' <> long "file" <> metavar "FILE" <> help "Save content to FILE"
    pure SinkArgs{..}

parseSelectionMode :: Parser SelectionMode
parseSelectionMode =
    foldr1 @[]
        (<|>)
        [ literal Area "area" "Select a region"
        , literal Window "window" "Select a visible window"
        , literal Output "output" "Select a visible display output"
        , literal Screen "screen" "All visible outputs"
        , literal Anything "anything" "Select a region, window, or output"
        , pure Anything
        ]

parseAreaSelector :: Parser AreaSelector
parseAreaSelector =
    ByName
        <$> strOption
            ( long "area-name"
                <> metavar "NAME"
                <> help "Retrieve an existing area or store a new one called NAME"
            )
            <|> flag' LastArea (long "last-area" <> help "Reuse the last selected area")
            <|> pure NewArea

parseSelectionArgs :: Parser SelectionArgs
parseSelectionArgs = do
    selectionMode <- parseSelectionMode
    areaSelector <- parseAreaSelector
    pure SelectionArgs{..}

parseCaptureAction :: Parser CaptureAction
parseCaptureAction =
    foldr1 @[]
        (<|>)
        [ literal Screenshot "screenshot" "Make a screenshot of the selected region"
        , literal Video "video" "Record a video of the selected region"
        , pure Screenshot
        ]

parseCaptureArgs :: Parser CaptureArgs
parseCaptureArgs = do
    captureAction <- parseCaptureAction
    delay <-
        optional . option auto $
            long "delay" <> metavar "T" <> help "Delay the capture by T seconds"
    duration <-
        optional . option auto $
            long "duration" <> metavar "T" <> help "Record video for T seconds"
    cursor <-
        switch $
            long "cursor" <> help "Include the mouse cursor in the screenshot"
    format <-
        optional . option (maybeReader formatFromExtension) $
            long "format"
                <> metavar "FORMAT"
                <> help
                    "Output format: png, jpg, or ppm for screenshots (default png); mp4 or webm for video (default mp4)"
    quality <-
        optional . option auto $
            long "quality"
                <> metavar "N"
                <> help "JPEG quality 0-100 (default: 80)"
    scale <-
        optional . option auto $
            long "scale" <> metavar "FACTOR" <> help "Scale factor for the screenshot"
    framerate <-
        optional . option auto $
            long "framerate" <> metavar "FPS" <> help "Video framerate"
    (audio, audioDevice) <-
        maybe (False, Nothing) (True,)
            <$> optional (audioFlag *> optional audioDeviceOption)
    pure CaptureArgs{..}
  where
    audioFlag :: Parser ()
    audioFlag = flag' () $ long "audio" <> help "Record audio with the video"
    audioDeviceOption :: Parser String
    audioDeviceOption =
        strOption $
            long "audio-device"
                <> metavar "DEVICE"
                <> help "Audio device to record from (requires --audio)"

data Args
    = Select SelectionArgs
    | Run SinkArgs SelectionArgs CaptureArgs

parseArgs :: Parser Args
parseArgs =
    ( hsubparser
        ( command "select" $
            info
                (Select <$> parseSelectionArgs)
                (fullDesc <> progDesc "Print the selected area and exit")
        )
        <|> (Run <$> parseSinkArgs <*> parseSelectionArgs <*> parseCaptureArgs)
    )
        <* simpleVersioner ("guzzle " <> showVersion Paths_guzzle.version)

parserInfo :: ParserInfo Args
parserInfo = info (parseArgs <**> helper) fullDesc

runParser :: IO Args
runParser = customExecParser defaultPrefs{prefBacktrack = Backtrack} parserInfo
