module Swaymsg where

import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.Maybe (fromMaybe, isJust)
import Region
import Prelude

data Rect = Rect
    { x :: Int
    , y :: Int
    , width :: Int
    , height :: Int
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)

data Tree = Tree
    { nodes :: [Tree]
    , floatingNodes :: [Tree]
    , rect :: Rect
    , pid :: Maybe Int
    , visible :: Maybe Bool
    , name :: Maybe Text
    }
    deriving stock (Generic)

instance FromJSON Tree where
    parseJSON =
        Aeson.genericParseJSON
            Aeson.defaultOptions{Aeson.fieldLabelModifier = Aeson.camelTo2 '_'}

windows :: Tree -> [Tree]
windows t@Tree{nodes, floatingNodes} = [t | isWindow t] <> mconcat (windows <$> nodes <> floatingNodes)
  where
    isWindow Tree{pid, visible} = isJust pid && isJust visible

treeToRegion :: Tree -> Region
treeToRegion Tree{rect = Rect{x, y, width = w, height = h}} = Region{..}

getTree :: IO Tree
getTree = jsonCmd ["swaymsg", "--raw", "--type", "get_tree"] ""

getWindows :: IO [Tree]
getWindows = windows <$> getTree

getVisibleWindows :: IO [Tree]
getVisibleWindows = filter (fromMaybe False . visible) <$> getWindows

getVisibleWindowRegions :: IO [Region]
getVisibleWindowRegions = treeToRegion <$$> getVisibleWindows

getScreenRegion :: IO Region
getScreenRegion = treeToRegion <$> getTree
