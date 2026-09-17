module Args where

import Capture
import Content (formatFromExtension)
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.List (intercalate, nub)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Data.Version (showVersion)
import Options.Applicative
import Options.Applicative.Types (Backtracking (..))
import Paths_guzzle qualified
import Region qualified
import Selection (SelectionArgs (..))
import Selection qualified
import Sink
import Template (Template)
import Template qualified
import Prelude

literal :: a -> NonEmpty String -> String -> Parser a
literal result tokens helpStr =
    flip
        argument
        ( metavar (NonEmpty.head tokens)
            <> help helpStr
            <> completeWith (toList tokens)
        )
        . maybeReader
        $ \arg -> if arg `elem` tokens then Just result else Nothing

parseSinkAction :: Parser SinkAction
parseSinkAction =
    foldr1 @[]
        (<|>)
        [ literal Copy ["copy"] "Copy contents to clipboard"
        , literal Save ["save"] "Save contents to file"
        , literal Print ["print"] "Print contents to stdout"
        ]

parseSinkArgs :: Parser SinkArgs
parseSinkArgs = do
    sinkAction <- optional parseSinkAction
    file <-
        optional . option (eitherReader $ first Text.unpack . Template.parse) $
            short 'f'
                <> long "file"
                <> metavar "TEMPLATE"
                <> help
                    "Save content to TEMPLATE; placeholders: %n name, %k kind, %o output, %a app, %p pid, %i index, %d timestamp, %x %y %w %h region, %% literal %"
    noNotify <-
        switch $ long "no-notify" <> help "Do not send desktop notifications"
    pure SinkArgs{..}

kindHelp :: Region.Kind -> String
kindHelp kind = description <> " (also: " <> intercalate ", " others <> ")"
  where
    others = Text.unpack <$> NonEmpty.tail (Region.aliases kind)
    description = case kind of
        Region.Area -> "Select a region"
        Region.Window -> "Select a visible window"
        Region.Output -> "Select a visible display output"
        Region.Screen -> "All visible outputs"

parseMode :: Parser Selection.Mode
parseMode =
    fromMaybe Selection.anything . NonEmpty.nonEmpty . nub . concat
        <$> many parseKind
  where
    parseKind =
        foldr1 @[] (<|>) $
            (kindParser <$> [minBound .. maxBound])
                <> [ literal
                        (toList Selection.anything)
                        ["anything"]
                        "Select a region, window, or output"
                   ]
    kindParser :: Region.Kind -> Parser [Region.Kind]
    kindParser kind =
        literal
            [kind]
            (Text.unpack <$> Region.aliases kind)
            (kindHelp kind)

parseAreaSelector :: Parser Selection.AreaSelector
parseAreaSelector =
    Selection.ByName
        <$> strOption
            ( long "area-name"
                <> metavar "NAME"
                <> help "Retrieve an existing area or store a new one called NAME"
            )
            <|> flag'
                Selection.LastArea
                (long "last-area" <> help "Reuse the last selected area")
            <|> pure Selection.NewArea

data SelectFormat = SelectFormat
    { fallback :: Maybe Template
    , byKind :: [(Region.Kind, Template)]
    }

templateFor :: SelectFormat -> Region.Kind -> Maybe Template
templateFor SelectFormat{..} kind = lookup kind byKind <|> fallback

templateOption :: String -> String -> Parser (Maybe Template)
templateOption name helpStr =
    optional . option (eitherReader $ first Text.unpack . Template.parse) $
        long name <> metavar "TEMPLATE" <> help helpStr

parseSelectFormat :: Parser SelectFormat
parseSelectFormat = do
    fallback <-
        templateOption
            "format"
            "Print TEMPLATE for each selected item instead of its region; same placeholders as --file"
    byKind <- catMaybes <$> traverse kindFormat [minBound .. maxBound]
    pure SelectFormat{..}
  where
    kindFormat kind =
        fmap (kind,)
            <$> templateOption
                (Text.unpack (NonEmpty.head $ Region.aliases kind) <> "-format")
                "Like --format, but only for items of this kind"

parseSelectionArgs :: Parser SelectionArgs
parseSelectionArgs = do
    mode <- parseMode
    areaSelector <- parseAreaSelector
    every <-
        switch $
            long "all"
                <> help "Select every candidate of the chosen kind without prompting"
    noHistory <-
        switch $
            long "no-history" <> help "Do not store or recall saved areas"
    pure SelectionArgs{..}

parseCaptureAction :: Parser CaptureAction
parseCaptureAction =
    foldr1 @[]
        (<|>)
        [ literal Screenshot ["screenshot"] "Make a screenshot of the selected region"
        , literal Video ["video"] "Record a video of the selected region"
        , pure Screenshot
        ]

positive :: String -> Either String Double
positive s = case reads s of
    [(factor, "")] | factor > 0 -> Right factor
    _ -> Left "must be a number greater than 0"

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
        optional . option (eitherReader positive) $
            long "scale"
                <> metavar "FACTOR"
                <> help "Scale factor for screenshots and videos, e.g. 2 or 0.5"
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
    = Select SelectionArgs SelectFormat
    | Run SinkArgs SelectionArgs CaptureArgs

parseVerbosity :: Parser Verbosity
parseVerbosity =
    flag' Warn (short 'q' <> long "quiet" <> help "Print only warnings and errors")
        <|> flag' Trace (long "debug" <> help "Print executed commands")
        <|> pure Info

parseArgs :: Parser (Verbosity, Args)
parseArgs =
    (,)
        <$> parseVerbosity
        <*> ( hsubparser
                ( command "select" $
                    info
                        (Select <$> parseSelectionArgs <*> parseSelectFormat)
                        (fullDesc <> progDesc "Print the selected area and exit")
                )
                <|> (Run <$> parseSinkArgs <*> parseSelectionArgs <*> parseCaptureArgs)
            )
        <* simpleVersioner ("guzzle " <> showVersion Paths_guzzle.version)

parserInfo :: ParserInfo (Verbosity, Args)
parserInfo = info (parseArgs <**> helper) fullDesc

runParser :: IO (Verbosity, Args)
runParser = customExecParser defaultPrefs{prefBacktrack = Backtrack} parserInfo
