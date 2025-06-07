{-# OPTIONS_GHC -Wno-partial-fields #-}

module Selection where

import Control.Exception (SomeException, try)
import Control.Monad.Extra (firstJustM, fromMaybeM)
import Data.Either.Extra (eitherToMaybe)
import Hyprctl qualified
import Options.Applicative
    ( Parser
    , argument
    , help
    , maybeReader
    , metavar
    , (<|>)
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

newtype SelectionArgs = SelectionArgs
    { selectionMode :: SelectionMode
    }

parseSelectionArgs :: Parser SelectionArgs
parseSelectionArgs = do
    selectionMode <- parseSelectionMode
    pure SelectionArgs{..}

selection :: SelectionArgs -> IO Region
selection SelectionArgs{selectionMode = Anything} = Slurp.selectAnything =<< getVisibleWindowRegions
selection SelectionArgs{selectionMode = Area} = Slurp.selectNewRegion
selection SelectionArgs{selectionMode = Window} = Slurp.selectRegion =<< getVisibleWindowRegions
selection SelectionArgs{selectionMode = Output} = Slurp.selectOutput
selection SelectionArgs{selectionMode = Screen} = getScreenRegion

getVisibleWindowRegions :: IO [Region]
getVisibleWindowRegions =
    fromMaybeM (fail "Cannot get visible window regions")
        . firstJustM (fmap eitherToMaybe . try @SomeException)
        $ [ Swaymsg.getVisibleWindowRegions
          , Hyprctl.getVisibleWindowRegions
          ]

getScreenRegion :: IO Region
getScreenRegion =
    fromMaybeM (fail "Cannot get screen information")
        . firstJustM (fmap eitherToMaybe . try @SomeException)
        $ [ Swaymsg.getScreenRegion
          ]
