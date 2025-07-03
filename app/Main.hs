module Main where

import Capture
import Control.Concurrent.Extra
import Control.Exception.Extra
import Control.Monad (join)
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
import Selection
import Sink
import System.Console.ANSI (showCursor)
import System.Exit (exitFailure)
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

abort :: AsyncException -> IO ()
abort _ = do
    showCursor
    exitFailure

main :: IO ()
main = flip catch abort . join . onceFork $ do
    createNamedRegionTable
    Args{..} <- customExecParser defaultPrefs{prefBacktrack = Backtrack} parserInfo
    selection selectionArgs >>= capture captureArgs >>= sink sinkArgs
