module Item where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Region (Region (..))
import Region qualified
import Prelude

data Item = Item
    { kind :: Region.Kind
    , region :: Region
    , name :: Text
    , output :: Maybe Text
    , app :: Maybe Text
    , pid :: Maybe Int
    , identifier :: Maybe Text
    }

data Desktop = Desktop
    { outputs :: [Item]
    , windows :: [Item]
    }

plain :: Region.Kind -> Text -> Region -> Item
plain kind name region =
    Item{output = Nothing, app = Nothing, pid = Nothing, identifier = Nothing, ..}

outputNamed :: Text -> Region -> Item
outputNamed name region = (plain Region.Output name region){output = Just name}

timestamp :: IO String
timestamp = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

placeholder :: String -> Int -> Item -> Char -> Maybe String
placeholder time index Item{region = Region{..}, ..} = \case
    'n' -> Just $ Text.unpack name
    'k' -> Just . Text.unpack . NonEmpty.head $ Region.aliases kind
    'o' -> Text.unpack <$> output
    'a' -> Text.unpack <$> app
    'p' -> show <$> pid
    'I' -> Text.unpack <$> identifier
    'i' -> Just $ show index
    'd' -> Just time
    'x' -> Just $ show x
    'y' -> Just $ show y
    'w' -> Just $ show w
    'h' -> Just $ show h
    _ -> Nothing
