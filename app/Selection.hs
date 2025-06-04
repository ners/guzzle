{-# OPTIONS_GHC -Wno-partial-fields #-}

module Selection where

import Prelude
import Region

data SelectionArgs = SelectionArgs
    {}

data Selection
    = Region Region
    | Window
    | Screen

selection :: SelectionArgs -> IO Selection
selection _ = pure Screen
