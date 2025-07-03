module Sink where

import Content
import Control.Monad (when)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601
import Options.Applicative
    ( Alternative ((<|>))
    , Parser
    , argument
    , help
    , long
    , maybeReader
    , metavar
    , optional
    , short
    , strOption
    )
import System.FilePath ((-<.>))
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

data SinkArgs = SinkArgs
    { sinkAction :: Maybe SinkAction
    , file :: Maybe FilePath
    }

parseSinkArgs :: Parser SinkArgs
parseSinkArgs = do
    sinkAction <- optional parseSinkAction
    file <-
        optional . strOption $
            short 'f' <> long "file" <> metavar "FILE" <> help "Save content to FILE"
    pure SinkArgs{..}

sink :: SinkArgs -> Content -> IO ()
sink SinkArgs{..} Content{..} = do
    filename <-
        maybe (("guzzle-" <>) . iso8601Show <$> getCurrentTime) pure file
            <&> (-<.> extension contentType)
    let hasFile = isJust file || sinkAction == Just Save || contentType == MP4
    when hasFile $ LazyByteString.writeFile filename content
    case sinkAction of
        Nothing -> LazyByteString.putStr content
        Just Print -> LazyByteString.putStr content
        Just Save -> pure ()
        Just Copy | hasFile -> WlCopy.wlCopyFile filename
        Just Copy -> WlCopy.wlCopy Content{..}
