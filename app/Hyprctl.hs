module Hyprctl where

import Prelude
import Region

newtype Workspace = Workspace { id :: Int }
    deriving stock (Generic)
    deriving newtype (Eq, Show)
    deriving anyclass (FromJSON)

newtype Monitor = Monitor { activeWorkspace :: Workspace }
    deriving stock (Generic)
    deriving newtype (Eq)
    deriving anyclass (FromJSON)

data Window = Window
    { at :: (Int, Int)
    , size :: (Int, Int)
    , workspace :: Workspace
    , title :: Text
    }
    deriving stock (Generic, Show)
    deriving anyclass (FromJSON)

getMonitors :: IO [Monitor]
getMonitors = jsonCmd ["hyprctl", "-j", "monitors"]

getWindows :: IO [Window]
getWindows = jsonCmd ["hyprctl", "-j", "clients"]

windowToRegion :: Window -> Region
windowToRegion Window{at = (x,y), size=(w,h)} = Region{..}

getVisibleWindows :: IO [Window]
getVisibleWindows = do
    monitors <- getMonitors
    windows <- getWindows
    let activeWorkspaces = activeWorkspace <$> monitors
    let visibleWindows = filter (\Window{workspace} -> workspace `elem` activeWorkspaces) windows
    pure visibleWindows

getVisibleWindowRegions :: IO [Region]
getVisibleWindowRegions = windowToRegion <$$> getVisibleWindows
