module Notify where

import Control.Exception (SomeException, try)
import Control.Monad.Extra (whenM)
import System.Environment (lookupEnv)
import Prelude

enabled :: IO Bool
enabled = maybe True (/= "0") <$> lookupEnv "GUZZLE_NOTIFY"

notify :: Text -> Text -> IO ()
notify summary body =
    whenM enabled . void . try @SomeException $
        cmd_ ("notify-send" :| ["-a", "guzzle", summary, body]) nullStream
