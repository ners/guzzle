{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Hyprctl where

import Data.Aeson qualified as Aeson
import Data.List (dropWhileEnd, find)
import Item (Item (..))
import Item qualified
import Region (Region (..))
import Region qualified
import Prelude hiding (id)

newtype Workspace = Workspace {id :: Int}
    deriving stock (Generic)
    deriving newtype (Eq, Show)
    deriving anyclass (FromJSON)

data Monitor = Monitor
    { id :: Int
    , name :: Text
    , x :: Int
    , y :: Int
    , width :: Int
    , height :: Int
    , scale :: Double
    , transform :: Int
    , activeWorkspace :: Workspace
    }
    deriving stock (Generic)
    deriving anyclass (FromJSON)

data Window = Window
    { at :: (Int, Int)
    , size :: (Int, Int)
    , workspace :: Workspace
    , monitor :: Int
    , title :: Text
    , class_ :: Text
    , pid :: Int
    , stableId :: Maybe Text
    }
    deriving stock (Generic, Show)

instance FromJSON Window where
    parseJSON =
        Aeson.genericParseJSON
            Aeson.defaultOptions
                { Aeson.fieldLabelModifier = dropWhileEnd (== '_')
                }

getMonitors :: IO [Monitor]
getMonitors = jsonCmd ["hyprctl", "-j", "monitors"] ""

getWindows :: IO [Window]
getWindows = jsonCmd ["hyprctl", "-j", "clients"] ""

windowToItem :: [Monitor] -> Window -> Item
windowToItem monitors window =
    Item
        { kind = Region.Window
        , region = Region{x, y, w, h}
        , name = window.title
        , output = (.name) <$> find ((== window.monitor) . (.id)) monitors
        , app = Just window.class_
        , pid = Just window.pid
        , identifier = window.stableId
        }
  where
    (x, y) = window.at
    (w, h) = window.size

monitorToItem :: Monitor -> Item
monitorToItem monitor =
    (Item.plain Region.Output monitor.name Region{x = monitor.x, y = monitor.y, w, h})
        { output = Just monitor.name
        }
  where
    (pixelWidth, pixelHeight)
        | odd monitor.transform = (monitor.height, monitor.width)
        | otherwise = (monitor.width, monitor.height)
    logical :: Int -> Int
    logical pixels = floor $ fromIntegral pixels / monitor.scale + 0.5
    w = logical pixelWidth
    h = logical pixelHeight

getDesktop :: IO Item.Desktop
getDesktop = do
    monitors <- getMonitors
    windows <- getWindows
    let activeWorkspaces = (.activeWorkspace) <$> monitors
    pure
        Item.Desktop
            { outputs = monitorToItem <$> monitors
            , windows =
                windowToItem monitors
                    <$> filter (\window -> window.workspace `elem` activeWorkspaces) windows
            }
