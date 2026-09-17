{-# OPTIONS_GHC -Wno-partial-fields #-}

module Selection where

import Control.Exception (SomeException, try)
import Control.Monad.Extra (fromMaybeM)
import Data.Either.Extra (eitherToMaybe)
import Data.Foldable.Extra (firstJustM, for_)
import Hyprctl qualified
import Niri qualified
import Persistence
    ( NamedRegion (..)
    , getAllRegions
    , getRegionByName
    , insertRegion
    )
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

data SelectionArgs = SelectionArgs
    { selectionMode :: SelectionMode
    , regionName :: Maybe Text
    }

selection :: SelectionArgs -> IO Region
selection args@SelectionArgs{..} =
    firstJustM getRegionByName regionName >>= flip maybe (pure . region) do
        region <- selectNewRegion args
        for_ regionName \name -> insertRegion NamedRegion{..}
        pure region

selectNewRegion :: SelectionArgs -> IO Region
selectNewRegion SelectionArgs{selectionMode = Anything} = Slurp.selectAnything =<< getVisibleWindowRegions
selectNewRegion SelectionArgs{selectionMode = Area, regionName = Nothing} = Slurp.selectNewOrExistingRegion =<< region <$$> getAllRegions
selectNewRegion SelectionArgs{selectionMode = Area} = Slurp.selectNewRegion
selectNewRegion SelectionArgs{selectionMode = Window} = Slurp.selectRegion =<< getVisibleWindowRegions
selectNewRegion SelectionArgs{selectionMode = Output} = Slurp.selectOutput
selectNewRegion SelectionArgs{selectionMode = Screen} = getScreenRegion

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
