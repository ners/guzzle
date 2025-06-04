module Swaymsg where

import Prelude
import Region

newtype Nodes a = Nodes { nodes :: [a] }
    deriving stock (Generic)
    deriving anyclass (FromJSON)

data Con = WindowCon Window | Con (Nodes Con)
    deriving stock (Generic)
    deriving anyclass (FromJSON)

windows :: Con -> [Window]
windows (WindowCon w) = pure w
windows (Con (Nodes {nodes})) = mconcat $ windows <$> nodes

data Rect = Rect
    { x :: Int
    , y :: Int
    , width :: Int
    , height :: Int
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)

data Window = Window
    { pid :: Int
    , visible :: Bool
    , rect :: Rect
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)

windowToRegion :: Window -> Region
windowToRegion Window{rect = Rect{x, y, width = w, height = h}} = Region{..}

getWindows :: IO [Window]
getWindows = windows <$> jsonCmd ["swaymsg", "--raw", "--type", "get_tree"]

getVisibleWindows :: IO [Window]
getVisibleWindows = filter visible <$> getWindows

getVisibleWindowRegions :: IO [Region]
getVisibleWindowRegions = windowToRegion <$$> getVisibleWindows
