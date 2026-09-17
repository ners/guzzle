module Main where

import Args
import Capture
import Control.Concurrent.Extra
import Control.Exception.Extra
import Control.Monad (join)
import Persistence (createNamedRegionTable)
import Selection
import Sink
import System.Console.ANSI
    ( hClearLine
    , hHideCursor
    , hSetCursorColumn
    , hShowCursor
    )
import System.Exit (exitSuccess)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stderr, stdout)
import Prelude

main :: IO ()
main = do
    createNamedRegionTable
    args <- runParser
    hHideCursor stderr
    hSetBuffering stdout NoBuffering
    result <-
        try . join . onceFork $
            case args of
                Select selectionArgs -> selection selectionArgs >>= putStrLn . show
                Run sinkArgs selectionArgs captureArgs ->
                    selection selectionArgs >>= capture captureArgs >>= sink sinkArgs
    hShowCursor stderr
    case result of
        Left (e :: SomeException) -> do
            hClearLine stderr
            hSetCursorColumn stderr 0
            fatalError $ ishow e
        Right () -> exitSuccess
