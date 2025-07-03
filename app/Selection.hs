{-# OPTIONS_GHC -Wno-partial-fields #-}

module Selection where

import Control.Exception (SomeException, try)
import Control.Monad.Extra (fromMaybeM)
import Data.Either.Extra (eitherToMaybe)
import Data.Foldable.Extra (firstJustM, for_)
import Hyprctl qualified
import Options.Applicative
    ( Parser
    , argument
    , help
    , long
    , maybeReader
    , metavar
    , optional
    , strOption
    , (<|>)
    )
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

parseSelectionMode :: Parser SelectionMode
parseSelectionMode =
    foldr1 @[]
        (<|>)
        [ modeArgument Area "area" "Select a region"
        , modeArgument Window "window" "Select a visible window"
        , modeArgument Output "output" "Select a visible display output"
        , modeArgument Screen "screen" "All visible outputs"
        , modeArgument Anything "anything" "Select a region, window, or output"
        , pure Anything
        ]
  where
    modeArgument
        :: SelectionMode
        -> String
        -> String
        -> Parser SelectionMode
    modeArgument mode str helpStr = flip argument (metavar str <> help helpStr) . maybeReader $
        \x -> if x == str then Just mode else Nothing

data SelectionArgs = SelectionArgs
    { selectionMode :: SelectionMode
    , regionName :: Maybe Text
    }

parseSelectionArgs :: Parser SelectionArgs
parseSelectionArgs = do
    selectionMode <- parseSelectionMode
    regionName <-
        optional . strOption $
            long "area-name"
                <> metavar "NAME"
                <> help "Retrieve an existing area or store a new one called NAME"
    pure SelectionArgs{..}

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
          ]

getScreenRegion :: IO Region
getScreenRegion =
    fromMaybeM (fail "Cannot get screen information")
        . firstJustM @[] (fmap eitherToMaybe . try @SomeException)
        $ [ Swaymsg.getScreenRegion
          ]
