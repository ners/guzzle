{-# OPTIONS_GHC -Wno-partial-fields #-}

module Selection where

import Control.Exception (SomeException, try)
import Control.Monad.Extra (fromMaybeM)
import Data.Either.Extra (eitherToMaybe)
import Data.Foldable.Extra (firstJustM, for_)
import Hyprctl qualified
import Niri qualified
import Persistence
import Region
import Slurp qualified
import Swaymsg qualified
import Prelude

data SelectionMode
    = Area
    | Window
    | Output
    | Screen
    | Anything
    deriving stock (Eq, Bounded, Enum)

data AreaSelector
    = ByName Text
    | LastArea
    | NewArea

data SelectionArgs = SelectionArgs
    { selectionMode :: SelectionMode
    , areaSelector :: AreaSelector
    }

selection :: SelectionArgs -> IO Region
selection SelectionArgs{areaSelector = LastArea} =
    fromMaybeM (fail "No last area saved") getLastRegion
selection args@SelectionArgs{..} = do
    let regionName = case areaSelector of ByName name -> Just name; _ -> Nothing
    region <-
        firstJustM getNamedRegion regionName >>= flip maybe (pure . region) do
            region <- selectNewRegion args
            for_ regionName \name -> insertNamedRegion NamedRegion{..}
            pure region
    setLastRegion region
    pure region

selectNewRegion :: SelectionArgs -> IO Region
selectNewRegion SelectionArgs{selectionMode = Anything} =
    Slurp.selectAnything
        =<< mconcatM
            [ getVisibleWindowRegions
            , getAllNamedRegions'
            , getLastRegion'
            ]
selectNewRegion SelectionArgs{selectionMode = Area, areaSelector = ByName{}} =
    Slurp.selectNewRegion
selectNewRegion SelectionArgs{selectionMode = Area} =
    Slurp.selectNewOrExistingRegion
        =<< mconcatM
            [ getAllNamedRegions'
            , getLastRegion'
            ]
selectNewRegion SelectionArgs{selectionMode = Window} =
    Slurp.selectRegion =<< getVisibleWindowRegions
selectNewRegion SelectionArgs{selectionMode = Output} = Slurp.selectOutput
selectNewRegion SelectionArgs{selectionMode = Screen} = getScreenRegion

getAllNamedRegions' :: IO [Region]
getAllNamedRegions' = region <$$> getAllNamedRegions

getLastRegion' :: IO [Region]
getLastRegion' = maybeToList <$> getLastRegion

getVisibleWindowRegions :: IO [Region]
getVisibleWindowRegions =
    fromMaybeM (fail "Cannot get visible window regions")
        . firstJustM @[] (fmap eitherToMaybe . try @SomeException)
        $ [ Swaymsg.getVisibleWindowRegions
          , Hyprctl.getVisibleWindowRegions
          , Niri.getVisibleWindowRegions
          ]

getScreenRegion :: IO Region
getScreenRegion =
    fromMaybeM (fail "Cannot get screen information")
        . firstJustM @[] (fmap eitherToMaybe . try @SomeException)
        $ [ Swaymsg.getScreenRegion
          , Niri.getScreenRegion
          ]
