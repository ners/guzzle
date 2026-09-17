module Main where

import Args
import Capture
import Control.Concurrent.Extra
import Control.Exception.Extra
import Control.Monad (join)
import Persistence (createNamedRegionTable)
import Selection
import Sink
import System.Console.ANSI (clearLine, hideCursor, setCursorColumn, showCursor)
import System.Exit (exitSuccess)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Prelude

main :: IO ()
main = do
    createNamedRegionTable
    args <- runParser
    hideCursor
    hSetBuffering stdout NoBuffering
    result <-
        try . join . onceFork $
            case args of
                Select selectionArgs -> selection selectionArgs >>= putStrLn . show
                Run sinkArgs selectionArgs captureArgs ->
                    selection selectionArgs >>= capture captureArgs >>= sink sinkArgs
    showCursor
    case result of
        Left (e :: SomeException) -> do
            clearLine
            setCursorColumn 0
            fatalError $ ishow e
        Right () -> exitSuccess
