{-# OPTIONS_GHC -Wno-partial-fields #-}

module Selection where

import Region
import Slurp qualified
import Swaymsg qualified
import Prelude

data SelectionMode
    = Anything
    | Area
    | Window
    | Output
    | Screen

data SelectionArgs = SelectionArgs
    { selectionMode :: SelectionMode
    }

selection :: SelectionArgs -> IO Region
selection SelectionArgs{selectionMode = Anything} = Slurp.selectAnything =<< Swaymsg.getVisibleWindowRegions
selection SelectionArgs{selectionMode = Area} = Slurp.selectNewRegion
selection SelectionArgs{selectionMode = Window} = Slurp.selectRegion =<< Swaymsg.getVisibleWindowRegions
selection SelectionArgs{selectionMode = Output} = Slurp.selectOutput
selection SelectionArgs{selectionMode = Screen} = Swaymsg.getScreenRegion
