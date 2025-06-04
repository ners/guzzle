module Prelude
    ( module Prelude
    , module Data.Aeson
    , module Data.ByteString
    , module Data.Functor
    , module Data.String
    , module Data.Text
    , module GHC.Generics
    )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import Data.Functor
import Data.Text (Text)
import GHC.Generics (Generic)
import "base" Prelude hiding (unzip)
import Data.List.NonEmpty (NonEmpty (..))
import System.Process.Typed (ExitCode (..))
import System.Process.Typed qualified as Process
import System.Console.ANSI
import System.IO (stderr)
import Data.Text.IO qualified as Text
import Data.Text qualified as Text
import Data.String (IsString(..))
import System.Exit (exitWith, exitFailure)
import Data.Text.Encoding qualified as Text
import Data.ByteString.Lazy qualified as LazyByteString
import Data.ByteString.Lazy (LazyByteString)
import Data.Aeson qualified as Aeson

infixl 4 <$$>

(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = fmap . fmap

fatalError :: Text -> IO a
fatalError t = printError t >> exitFailure

printError :: Text -> IO ()
printError t = do
    hSetSGR stderr [SetColor Foreground Vivid Red]
    Text.hPutStrLn stderr t
    hSetSGR stderr [Reset]

printWarn :: Text -> IO ()
printWarn t = do
    hSetSGR stderr [SetColor Foreground Vivid Yellow]
    Text.hPutStrLn stderr t
    hSetSGR stderr [Reset]

printInfo :: Text -> IO ()
printInfo t = do
    hSetSGR stderr [SetColor Foreground Dull Cyan]
    Text.hPutStrLn stderr t
    hSetSGR stderr [Reset]

printDebug :: Text -> IO ()
printDebug t = do
    hSetSGR stderr [SetColor Foreground Dull Magenta]
    Text.hPutStrLn stderr t
    hSetSGR stderr [Reset]

cmd' :: NonEmpty Text -> IO (ExitCode, LazyByteString, LazyByteString)
cmd' (x :| xs) = do
    printInfo $ Text.unwords (x : xs)
    (exitCode, out, err) <-
        Process.readProcess $ Process.proc (Text.unpack x) (Text.unpack <$> xs)
    pure (exitCode, out, err)

cmd :: NonEmpty Text -> IO Text
cmd xs =
    cmd' xs >>= \case
        (ExitSuccess, out, _) -> pure . Text.decodeUtf8 . LazyByteString.toStrict $ out
        (code, _, _) -> exitWith code

jsonCmd :: FromJSON a => NonEmpty Text -> IO a
jsonCmd xs =
    cmd' xs >>= \case
        (ExitSuccess, out, _) -> either (fatalError . fromString) pure . Aeson.eitherDecode $ out
        (code, _, _) -> exitWith code

cmd_ :: NonEmpty Text -> IO ()
cmd_ = void . cmd
