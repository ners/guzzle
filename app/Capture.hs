{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Capture where

import Content
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (guard)
import Data.Fixed (Micro)
import Data.Foldable (for_)
import Data.List.NonEmpty qualified as NonEmpty
import Grim qualified
import Item (Item)
import Item qualified
import Template (Template)
import Template qualified
import WfRecorder qualified
import Prelude

data CaptureAction
    = Screenshot
    | Video
    deriving stock (Eq)

data CaptureArgs = CaptureArgs
    { captureAction :: CaptureAction
    , delay :: Maybe Micro
    , duration :: Maybe Micro
    , cursor :: Bool
    , audio :: Bool
    , audioDevice :: Maybe String
    , format :: Maybe ContentType
    , quality :: Maybe Int
    , scale :: Maybe Double
    , framerate :: Maybe Int
    }

resolveFormat :: CaptureArgs -> Maybe Template -> IO ContentType
resolveFormat CaptureArgs{..} template = case format of
    Just format | elem @[] format allowed -> pure format
    Just _ -> fatalError "Invalid --format for this capture mode"
    Nothing -> pure . fromMaybe def $ do
        format <- formatFromExtension =<< Template.extension =<< template
        format <$ guard (elem @[] format allowed)
  where
    (allowed, def) = case captureAction of
        Screenshot -> ([PNG, JPEG, PPM], PNG)
        Video -> ([MP4, WEBM], MP4)

capture
    :: CaptureArgs
    -> ContentType
    -> NonEmpty Item
    -> IO (NonEmpty (Item, Content))
capture CaptureArgs{..} contentType items = do
    for_ delay $ countdown "Starting in: "
    contents <- case captureAction of
        Screenshot ->
            mapConcurrently
                (Grim.screenshotRegion contentType cursor quality scale . Item.region)
                items
        Video ->
            WfRecorder.recordRegions
                contentType
                audio
                audioDevice
                framerate
                scale
                duration
                (Item.region <$> items)
    pure . NonEmpty.zip items $ contents <&> \content -> Content{..}
