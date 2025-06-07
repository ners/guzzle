module Main where

import Capture
import Options.Applicative
    ( Parser
    , ParserInfo
    , execParser
    , fullDesc
    , helper
    , info
    , progDesc
    )
import Selection
import Sink
import Prelude

data Args = Args
    { selectionArgs :: SelectionArgs
    , captureArgs :: CaptureArgs
    , sinkArgs :: SinkArgs
    }

parseArgs :: Parser Args
parseArgs = do
    sinkArgs <- parseSinkArgs
    selectionArgs <- parseSelectionArgs
    captureArgs <- parseCaptureArgs
    pure Args{..}

parserInfo :: ParserInfo Args
parserInfo = info (helper <*> parseArgs) (fullDesc <> progDesc "guzzle")

main :: IO ()
main = do
    Args{..} <- execParser parserInfo
    selection selectionArgs >>= capture captureArgs >>= sink sinkArgs
