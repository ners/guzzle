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
import Persistence (createNamedRegionTable)
import Prelude
import Selection
import Sink

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
    createNamedRegionTable
    Args{..} <- customExecParser defaultPrefs{prefBacktrack = Backtrack} parserInfo
    selection selectionArgs >>= capture captureArgs >>= sink sinkArgs
