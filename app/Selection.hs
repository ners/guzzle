{-# OPTIONS_GHC -Wno-partial-fields #-}

module Selection where

import Control.Exception (SomeException, try)
import Control.Monad ((>=>))
import Control.Monad.Extra (firstJustM, fromMaybeM)
import Data.Either.Extra (eitherToMaybe)
import Hyprctl qualified
import Options.Applicative
    ( ArgumentFields
    , Mod
    , Parser
    , argument
    , help
    , maybeReader
    , metavar
    , (<|>)
    )
import Region
import Slurp qualified
import Swaymsg qualified
import Text.ParserCombinators.ReadP qualified as ReadP
import Text.ParserCombinators.ReadPrec qualified as ReadPrec
import Text.Read
import Prelude

data SelectionMode
    = Area
    | Window
    | Output
    | Screen
    | Anything
    deriving stock (Eq, Bounded, Enum)

instance Show SelectionMode where
    show Area = "area"
    show Window = "window"
    show Output = "output"
    show Screen = "screen"
    show Anything = "anything"

instance Read SelectionMode where
    readPrec =
        foldr @[] @SelectionMode
            (\s acc -> readSelectionMode s <|> acc)
            (pure Anything)
            [minBound .. maxBound]
      where
        readSelectionMode :: SelectionMode -> ReadPrec SelectionMode
        readSelectionMode mode = ReadPrec.lift (ReadP.string $ show mode) $> mode

parseSelectionMode :: Parser SelectionMode
parseSelectionMode =
    foldr @[] (\m acc -> modeArgument m (modeFields m) <|> acc) (pure Anything) $
        [minBound .. maxBound]
  where
    modeFields :: SelectionMode -> Mod ArgumentFields SelectionMode
    modeFields mode = metavar (show mode) <> help (modeHelp mode)
    modeHelp :: SelectionMode -> String
    modeHelp Area = "Select a region"
    modeHelp Window = "Select a visible window"
    modeHelp Output = "Select a visible display output"
    modeHelp Screen = "All visible outputs"
    modeHelp Anything = "Select a region, window, or output"
    modeArgument
        :: SelectionMode -> Mod ArgumentFields SelectionMode -> Parser SelectionMode
    modeArgument mode =
        argument . maybeReader $
            readMaybe >=> \x ->
                if x == mode then Just mode else Nothing

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
    fromMaybeM (pure []) . firstJustM (fmap eitherToMaybe . try @SomeException) $
        [ Swaymsg.getVisibleWindowRegions
        , Hyprctl.getVisibleWindowRegions
        ]

getScreenRegion :: IO Region
getScreenRegion =
    fromMaybeM (error "Cannot get screen information")
        . firstJustM (fmap eitherToMaybe . try @SomeException)
        $ [ Swaymsg.getScreenRegion
          ]
