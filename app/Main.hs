module Main where

import Capture
import Options.Applicative
    ( Parser
    , ParserInfo
    , ParserPrefs (..)
    , customExecParser
    , fullDesc
    , helper
    , info
    , progDesc
    )
import Options.Applicative.Builder (defaultPrefs)
import Options.Applicative.Types (Backtracking (..))
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
    Args{..} <- customExecParser defaultPrefs{prefBacktrack = Backtrack} parserInfo
    selection selectionArgs >>= capture captureArgs >>= sink sinkArgs
