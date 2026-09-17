{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Niri where

import Control.Monad (guard)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List (find)
import Item (Item (..))
import Item qualified
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
    { id :: Int
    , workspaceId :: Maybe Int
    , layout :: WindowLayout
    , title :: Maybe Text
    , appId :: Maybe Text
    , pid :: Maybe Int
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

windowToItem :: [Workspace] -> [Output] -> Window -> Maybe Item
windowToItem workspaces outputs window = do
    wid <- window.workspaceId
    workspace <- find ((== wid) . (.id)) workspaces
    guard workspace.isActive
    outputName <- workspace.output
    out <- find ((== outputName) . (.name)) outputs
    LogicalOutput{x = ox, y = oy} <- out.logical
    (tx, ty) <- window.layout.tilePosInWorkspaceView
    let (tw, th) = window.layout.tileSize
    pure
        Item
            { kind = Region.Window
            , region =
                Region{x = ox + round tx, y = oy + round ty, w = round tw, h = round th}
            , name = fromMaybe "" window.title
            , output = Just outputName
            , app = window.appId
            , pid = window.pid
            , identifier = Just . ishow $ window.id
            }

outputToItem :: Output -> Maybe Item
outputToItem out =
    out.logical <&> \logical ->
        Item
            { kind = Region.Output
            , region = logicalToRegion logical
            , name = out.name
            , output = Just out.name
            , app = Nothing
            , pid = Nothing
            , identifier = Nothing
            }

getDesktop :: IO Item.Desktop
getDesktop = do
    workspaces <- getWorkspaces
    outputs <- getOutputs
    windows <- getWindows
    pure
        Item.Desktop
            { outputs = mapMaybe outputToItem outputs
            , windows = mapMaybe (windowToItem workspaces outputs) windows
            }
