module Args where

import Capture
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

parseSelectionArgs :: Parser SelectionArgs
parseSelectionArgs = do
    selectionMode <- parseSelectionMode
    regionName <-
        optional . strOption $
            long "area-name"
                <> metavar "NAME"
                <> help "Retrieve an existing area or store a new one called NAME"
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
    pure CaptureArgs{..}

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
