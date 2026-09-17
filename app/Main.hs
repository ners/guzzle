module Main where

import Args
import Capture
import Content (isVideo)
import Control.Concurrent.Extra
import Control.Exception.Extra
import Control.Monad (join)
import Control.Monad.Extra (whenM)
import Data.Foldable (for_)
import Data.List.NonEmpty qualified as NonEmpty
import Item qualified
import Selection (selection, validate)
import Sink
import System.Console.ANSI
    ( hClearLine
    , hHideCursor
    , hSetCursorColumn
    , hShowCursor
    )
import System.Exit (exitSuccess)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stderr, stdout)
import Template qualified
import Prelude

main :: IO ()
main = do
    (level, args) <- runParser
    setVerbosity level
    either fatalError pure . validate $ case args of
        Select selectionArgs _ -> selectionArgs
        Run _ selectionArgs _ -> selectionArgs
    whenM (verbosityAtLeast Info) $ hHideCursor stderr
    hSetBuffering stdout NoBuffering
    result <-
        try . join . onceFork $
            case args of
                Select selectionArgs formats -> do
                    items <- selection selectionArgs
                    time <- Item.timestamp
                    for_ (NonEmpty.zip (1 :| [2 ..]) items) \(index, item) ->
                        putStrLn $ case templateFor formats (Item.kind item) of
                            Nothing -> show $ Item.region item
                            Just template ->
                                Template.expand (Item.placeholder time index item) template
                Run sinkArgs selectionArgs captureArgs -> do
                    contentType <- resolveFormat captureArgs (file sinkArgs)
                    items <- selection selectionArgs
                    when
                        ( sinkAction sinkArgs == Just Print
                            && isVideo contentType
                            && length items > 1
                        )
                        $ fatalError "print does not support multiple videos"
                    capture captureArgs contentType items >>= sink sinkArgs
    whenM (verbosityAtLeast Info) $ hShowCursor stderr
    case result of
        Left (e :: SomeException) -> do
            whenM (verbosityAtLeast Info) do
                hClearLine stderr
                hSetCursorColumn stderr 0
            fatalError $ ishow e
        Right () -> exitSuccess
