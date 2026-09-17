{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Swaymsg where

import Control.Applicative ((<|>))
import Data.Aeson qualified as Aeson
import Data.List (dropWhileEnd)
import Item (Item (..))
import Item qualified
import Region (Region (..))
import Region qualified
import Prelude

data Rect = Rect
    { x :: Int
    , y :: Int
    , width :: Int
    , height :: Int
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)

newtype WindowProperties = WindowProperties
    { class_ :: Maybe Text
    }
    deriving stock (Generic)

instance FromJSON WindowProperties where
    parseJSON =
        Aeson.genericParseJSON
            Aeson.defaultOptions
                { Aeson.fieldLabelModifier = dropWhileEnd (== '_')
                }

data Tree = Tree
    { nodes :: [Tree]
    , floatingNodes :: [Tree]
    , rect :: Rect
    , pid :: Maybe Int
    , visible :: Maybe Bool
    , name :: Maybe Text
    , appId :: Maybe Text
    , foreignToplevelIdentifier :: Maybe Text
    , windowProperties :: Maybe WindowProperties
    }
    deriving stock (Generic)

instance FromJSON Tree where
    parseJSON =
        Aeson.genericParseJSON
            Aeson.defaultOptions{Aeson.fieldLabelModifier = Aeson.camelTo2 '_'}

windows :: Tree -> [Tree]
windows t = [t | isWindow t] <> foldMap windows (t.nodes <> t.floatingNodes)
  where
    isWindow Tree{pid, visible} = isJust pid && isJust visible

outputs :: Tree -> [Tree]
outputs root = filter ((/= Just "__i3") . (.name)) root.nodes

treeToRegion :: Tree -> Region
treeToRegion Tree{rect = Rect{x, y, width = w, height = h}} = Region{..}

outputItem :: Tree -> Item
outputItem tree =
    Item
        { kind = Region.Output
        , region = treeToRegion tree
        , name = fromMaybe "" tree.name
        , output = tree.name
        , app = Nothing
        , pid = Nothing
        , identifier = Nothing
        }

windowItem :: Tree -> Tree -> Item
windowItem outputTree window =
    Item
        { kind = Region.Window
        , region = treeToRegion window
        , name = fromMaybe "" window.name
        , output = outputTree.name
        , app = window.appId <|> (window.windowProperties >>= (.class_))
        , pid = window.pid
        , identifier = window.foreignToplevelIdentifier
        }

getTree :: IO Tree
getTree = jsonCmd ["swaymsg", "--raw", "--type", "get_tree"] ""

getDesktop :: IO Item.Desktop
getDesktop = do
    tree <- getTree
    pure
        Item.Desktop
            { outputs = outputItem <$> outputs tree
            , windows =
                outputs tree >>= \outputTree ->
                    windowItem outputTree
                        <$> filter (fromMaybe False . (.visible)) (windows outputTree)
            }
