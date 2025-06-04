module Prelude
    ( module Prelude
    , module Data.Aeson
    , module Data.ByteString
    , module Data.Functor
    , module Data.List.NonEmpty
    , module Data.String
    , module Data.Text
    , module GHC.Generics
    )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Functor
import Data.List.NonEmpty (NonEmpty (..))
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.IO qualified as Text
import GHC.Generics (Generic)
import System.Console.ANSI
import System.Exit (exitFailure, exitWith)
import System.IO (stderr)
import System.Process.Typed (ExitCode (..))
import System.Process.Typed qualified as Process
import "base" Prelude hiding (unzip)

infixl 4 <$$>

(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = fmap . fmap

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

textInput :: Text -> Process.StreamSpec 'Process.STInput ()
textInput "" = Process.nullStream
textInput t = Process.byteStringInput . LazyByteString.fromStrict . Text.encodeUtf8 $ t

cmd' :: NonEmpty Text -> Text -> IO (ExitCode, LazyByteString, LazyByteString)
cmd' (x :| xs) (textInput -> input) = do
    printInfo $ Text.unwords (x : xs)
    (exitCode, out, err) <-
        Process.readProcess . Process.setStdin input $
            Process.proc (Text.unpack x) (Text.unpack <$> xs)
    pure (exitCode, out, err)

cmd :: NonEmpty Text -> Text -> IO Text
cmd xs input =
    cmd' xs input >>= \case
        (ExitSuccess, out, _) -> pure . Text.decodeUtf8 . LazyByteString.toStrict $ out
        (code, _, _) -> exitWith code

jsonCmd :: (FromJSON a) => NonEmpty Text -> Text -> IO a
jsonCmd xs input =
    cmd' xs input >>= \case
        (ExitSuccess, out, _) -> either (fatalError . fromString) pure . Aeson.eitherDecode $ out
        (code, _, _) -> exitWith code

cmd_ :: NonEmpty Text -> Text -> IO ()
cmd_ = (void .) . cmd
