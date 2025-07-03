module Sink where

import Content
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Time (getCurrentTime)
import Options.Applicative
    ( Alternative ((<|>))
    , Parser
    , argument
    , help
    , maybeReader
    , metavar
    )
import System.IO (hFlush, stdout)
import WlCopy qualified
import Prelude

data SinkAction
    = Copy
    | Save
    | Print
    deriving stock (Eq)

parseSinkAction :: Parser SinkAction
parseSinkAction =
    foldr1 @[]
        (<|>)
        [ sinkArgument Copy "copy" "Copy contents to clipboard"
        , sinkArgument Save "save" "Save contents to file"
        , sinkArgument Print "print" "Print contents to stdout"
        , pure Print
        ]
  where
    sinkArgument
        :: SinkAction
        -> String
        -> String
        -> Parser SinkAction
    sinkArgument s str helpStr =
        flip argument (metavar str <> help helpStr) . maybeReader $ \str' ->
            if str' == str then Just s else Nothing

newtype SinkArgs = SinkArgs
    { sinkAction :: SinkAction
    }

parseSinkArgs :: Parser SinkArgs
parseSinkArgs = do
    sinkAction <- parseSinkAction
    pure SinkArgs{..}

sink :: SinkArgs -> Content -> IO ()
sink SinkArgs{sinkAction = Copy} Content{..} = WlCopy.wlCopy content
sink SinkArgs{sinkAction = Save} Content{..} = do
    time <- getCurrentTime
    LazyByteString.writeFile (filename contentType $ show time) content
sink SinkArgs{sinkAction = Print} Content{..} = do
    LazyByteString.putStr content
    hFlush stdout
