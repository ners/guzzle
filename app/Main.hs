module Main where

import Capture
import Control.Concurrent.Extra
import Control.Exception.Extra
import Control.Monad (join)
import Data.Version (showVersion)
import Options.Applicative
    ( Parser
    , ParserInfo
    , ParserPrefs (..)
    , customExecParser
    , fullDesc
    , helper
    , info
    , progDesc
    , simpleVersioner
    )
import Options.Applicative.Builder (defaultPrefs)
import Options.Applicative.Types (Backtracking (..))
import Paths_guzzle qualified
import Persistence (createNamedRegionTable)
import Selection
import Sink
import System.Console.ANSI (clearLine, hideCursor, setCursorColumn, showCursor)
import System.Exit (exitSuccess)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
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
    simpleVersioner $ "guzzle " <> showVersion Paths_guzzle.version
    pure Args{..}

parserInfo :: ParserInfo Args
parserInfo = info (helper <*> parseArgs) (fullDesc <> progDesc "guzzle")

main :: IO ()
main = do
    createNamedRegionTable
    Args{..} <- customExecParser defaultPrefs{prefBacktrack = Backtrack} parserInfo
    hideCursor
    hSetBuffering stdout NoBuffering
    result <-
        try . join . onceFork $
            selection selectionArgs >>= capture captureArgs >>= sink sinkArgs
    showCursor
    case result of
        Left (e :: SomeException) -> do
            clearLine
            setCursorColumn 0
            fatalError $ ishow e
        Right () -> exitSuccess
