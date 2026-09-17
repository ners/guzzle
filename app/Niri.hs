module Niri where

import Control.Monad (guard)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List (find, foldl1')
import Region (Region (Region))
import Region qualified
import Prelude hiding (id)

data WindowLayout = WindowLayout
    { tileSize :: (Double, Double)
    , tilePosInWorkspaceView :: Maybe (Double, Double)
    }
    deriving stock (Generic)

instance FromJSON WindowLayout where
    parseJSON =
        Aeson.genericParseJSON
            Aeson.defaultOptions{Aeson.fieldLabelModifier = Aeson.camelTo2 '_'}

data Window = Window
    { workspaceId :: Maybe Int
    , layout :: WindowLayout
    }
    deriving stock (Generic)

instance FromJSON Window where
    parseJSON =
        Aeson.genericParseJSON
            Aeson.defaultOptions{Aeson.fieldLabelModifier = Aeson.camelTo2 '_'}

data Workspace = Workspace
    { id :: Int
    , output :: Maybe Text
    , isActive :: Bool
    }
    deriving stock (Generic)

instance FromJSON Workspace where
    parseJSON =
        Aeson.genericParseJSON
            Aeson.defaultOptions{Aeson.fieldLabelModifier = Aeson.camelTo2 '_'}

data LogicalOutput = LogicalOutput
    { x :: Int
    , y :: Int
    , width :: Int
    , height :: Int
    }
    deriving stock (Generic)

instance FromJSON LogicalOutput where
    parseJSON =
        Aeson.genericParseJSON
            Aeson.defaultOptions{Aeson.fieldLabelModifier = Aeson.camelTo2 '_'}

data Output = Output
    { name :: Text
    , logical :: Maybe LogicalOutput
    }
    deriving stock (Generic)

instance FromJSON Output where
    parseJSON =
        Aeson.genericParseJSON
            Aeson.defaultOptions{Aeson.fieldLabelModifier = Aeson.camelTo2 '_'}

newtype Outputs = Outputs {outputs :: [Output]}

instance FromJSON Outputs where
    parseJSON = Aeson.withObject "Outputs" $ \obj ->
        Outputs <$> traverse Aeson.parseJSON (KeyMap.elems obj)

getWindows :: IO [Window]
getWindows = jsonCmd ["niri", "msg", "--json", "windows"] ""

getWorkspaces :: IO [Workspace]
getWorkspaces = jsonCmd ["niri", "msg", "--json", "workspaces"] ""

getOutputs :: IO [Output]
getOutputs = outputs <$> jsonCmd ["niri", "msg", "--json", "outputs"] ""

logicalToRegion :: LogicalOutput -> Region
logicalToRegion LogicalOutput{x, y, width, height} = Region{x, y, w = width, h = height}

windowToRegion :: [Workspace] -> [Output] -> Window -> Maybe Region
windowToRegion workspaces outputs Window{workspaceId, layout} = do
    wid <- workspaceId
    workspace <- find ((== wid) . id) workspaces
    guard (isActive workspace)
    outputName <- output workspace
    out <- find ((== outputName) . name) outputs
    LogicalOutput{x = ox, y = oy} <- logical out
    (tx, ty) <- tilePosInWorkspaceView layout
    let (tw, th) = tileSize layout
    pure Region{x = ox + round tx, y = oy + round ty, w = round tw, h = round th}

getVisibleWindowRegions :: IO [Region]
getVisibleWindowRegions = do
    workspaces <- getWorkspaces
    outputs <- getOutputs
    windows <- getWindows
    pure $ mapMaybe (windowToRegion workspaces outputs) windows

getScreenRegion :: IO Region
getScreenRegion = do
    logicals <- mapMaybe logical <$> getOutputs
    case logicalToRegion <$> logicals of
        [] -> fail "No outputs"
        rs -> pure $ foldl1' (<>) rs
