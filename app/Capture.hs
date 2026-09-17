{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Capture where

import Content
import Data.Fixed (Micro)
import Data.Foldable (for_)
import Grim qualified
import Region
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

capture :: CaptureArgs -> Region -> IO Content
capture CaptureArgs{..} region = do
    for_ delay $ countdown "Starting in: "
    case captureAction of
        Screenshot -> do
            contentType <- validateFormat [PNG, JPEG, PPM] PNG
            content <- Grim.screenshotRegion contentType cursor quality scale region
            pure Content{..}
        Video -> do
            contentType <- validateFormat [MP4, WEBM] MP4
            content <-
                WfRecorder.recordRegion contentType audio audioDevice framerate duration region
            pure Content{..}
  where
    validateFormat :: [ContentType] -> ContentType -> IO ContentType
    validateFormat allowed def = case format of
        Nothing -> pure def
        Just fmt | fmt `elem` allowed -> pure fmt
        Just _ -> fatalError "Invalid --format for this capture mode"
