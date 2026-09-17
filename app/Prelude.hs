module Prelude
    ( module Prelude
    , module Control.Monad
    , module Data.Aeson
    , module Data.ByteString
    , module Data.ByteString.Lazy
    , module Data.Function
    , module Data.Functor
    , module Data.List.NonEmpty
    , module Data.Maybe
    , module Data.String
    , module Data.Text
    , module GHC.Generics
    , module System.Process.Typed
    )
where

import Control.Concurrent (threadDelay)
import Control.Monad (unless, when, (<=<), (>=>))
import Control.Monad.Extra (ifM, mconcatMapM, notM, whenM)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Fixed (Micro, showFixed)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Functor
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import GHC.Generics (Generic)
import System.Console.ANSI
import System.Exit (exitFailure, exitWith)
import System.IO (hPutStr, stderr)
import System.IO.Unsafe (unsafePerformIO)
import System.Process.Typed (ExitCode (..), StreamSpec, nullStream)
import System.Process.Typed qualified as Process
import "base" Prelude hiding (unzip)

infixl 4 <$$>

(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = fmap . fmap

mconcatM :: (Monad m, Monoid a) => [m a] -> m a
mconcatM = mconcatMapM id

ishow :: (Show a, IsString s) => a -> s
ishow = fromString . show

fromText :: (IsString s) => Text -> s
fromText = fromString . Text.unpack

fatalError :: Text -> IO a
fatalError t = printError t >> exitFailure

printError :: Text -> IO ()
printError t = do
    hSetSGR stderr [SetColor Foreground Vivid Red]
    Text.hPutStrLn stderr t
    hSetSGR stderr [Reset]

data Verbosity = Warn | Info | Trace
    deriving stock (Eq, Ord, Bounded, Enum)

verbosity :: IORef Verbosity
verbosity = unsafePerformIO $ newIORef Info
{-# NOINLINE verbosity #-}

setVerbosity :: Verbosity -> IO ()
setVerbosity = writeIORef verbosity

verbosityAtLeast :: Verbosity -> IO Bool
verbosityAtLeast level = (level <=) <$> readIORef verbosity

verbosityBelow :: Verbosity -> IO Bool
verbosityBelow = notM . verbosityAtLeast

printWarn :: Text -> IO ()
printWarn t = do
    hSetSGR stderr [SetColor Foreground Vivid Yellow]
    Text.hPutStrLn stderr t
    hSetSGR stderr [Reset]

printInfo :: Text -> IO ()
printInfo t = whenM (verbosityAtLeast Info) do
    hSetSGR stderr [SetColor Foreground Dull Cyan]
    Text.hPutStrLn stderr t
    hSetSGR stderr [Reset]

printDebug :: Text -> IO ()
printDebug t = whenM (verbosityAtLeast Trace) do
    hSetSGR stderr [SetColor Foreground Dull Magenta]
    Text.hPutStrLn stderr t
    hSetSGR stderr [Reset]

textInput :: Text -> StreamSpec 'Process.STInput ()
textInput = Process.byteStringInput . LazyByteString.fromStrict . Text.encodeUtf8

cmd'
    :: NonEmpty Text
    -> StreamSpec 'Process.STInput ()
    -> IO (ExitCode, LazyByteString, LazyByteString)
cmd' (x :| xs) input = do
    printDebug $ Text.unwords (x : xs)
    (exitCode, out, err) <-
        Process.readProcess . Process.setStdin input $
            Process.proc (Text.unpack x) (Text.unpack <$> xs)
    pure (exitCode, out, err)

cmd :: NonEmpty Text -> StreamSpec 'Process.STInput () -> IO LazyByteString
cmd xs input =
    cmd' xs input >>= \case
        (ExitSuccess, out, _) -> pure out
        (code, _, _) -> exitWith code

textCmd :: NonEmpty Text -> StreamSpec 'Process.STInput () -> IO Text
textCmd xs input = Text.decodeUtf8 . LazyByteString.toStrict <$> cmd xs input

jsonCmd
    :: (FromJSON a) => NonEmpty Text -> StreamSpec 'Process.STInput () -> IO a
jsonCmd xs input =
    either (fatalError . fromString) pure . Aeson.eitherDecode
        =<< cmd xs input

cmd_ :: NonEmpty Text -> StreamSpec 'Process.STInput () -> IO ()
cmd_ (x :| xs) input = do
    printDebug $ Text.unwords (x : xs)
    Process.runProcess_ . Process.setStdin input $
        Process.proc (Text.unpack x) (Text.unpack <$> xs)

countdown :: String -> Micro -> IO ()
countdown what t = ifM (verbosityBelow Info) (sleep t) do
    for_ @[] [t, t - dt .. dt] \t' -> do
        hClearLine stderr
        hPutStr stderr $ what <> showFixed True t'
        hSetCursorColumn stderr 0
        sleep dt
    hClearLine stderr
  where
    sleep :: Micro -> IO ()
    sleep = threadDelay . round . (* 1_000_000)
    dt :: Micro
    dt = 0.01
