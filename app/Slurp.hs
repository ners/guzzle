module Slurp where

import Data.Text qualified as Text
import Region (Region (..))
import Text.Read (readMaybe)
import Prelude

data Pick
    = Listed Int
    | Output Text
    | Drawn
    deriving stock (Eq, Show)

parse :: Text -> Maybe (Region, Pick)
parse (Text.words -> position : size : label) = do
    region <- readMaybe . Text.unpack $ position <> " " <> size
    (region,) <$> case Text.unwords label of
        "" -> Just Drawn
        text
            | Just index <- Text.stripPrefix "#" text ->
                Listed <$> readMaybe (Text.unpack index)
            | otherwise -> Just $ Output text
parse _ = Nothing

pick :: [Text] -> [Region] -> IO (Region, Pick)
pick flags regions =
    textCmd
        ("slurp" :| "-d" : "-f" : "%x,%y %wx%h %l" : flags)
        (textInput candidates)
        >>= maybe (fail "Cannot parse slurp output") pure . parse
  where
    candidates =
        Text.unlines $
            zipWith
                (\index region -> ishow region <> " #" <> ishow index)
                [0 :: Int ..]
                regions
