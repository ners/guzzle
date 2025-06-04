module Sink where

import Prelude

data SinkArgs = SinkArgs
    {}

sink :: SinkArgs -> ByteString -> IO ()
sink _ _ = putStrLn "sink"
