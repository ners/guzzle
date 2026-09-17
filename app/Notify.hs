module Notify where

import Control.Exception (SomeException, try)
import Prelude

notify :: Text -> Text -> IO ()
notify summary body =
    void . try @SomeException $
        cmd_ ("notify-send" :| ["-a", "guzzle", summary, body]) nullStream
