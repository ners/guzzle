module Region where

import Prelude
import Text.Read
import Text.ParserCombinators.ReadP qualified as ReadP
import Text.ParserCombinators.ReadPrec qualified as ReadPrec

data Region = Region
    { x :: Int
    , y :: Int
    , w :: Int
    , h :: Int
    }

instance Read Region where
    readPrec = do
        x <- readPrec
        ReadPrec.lift $ ReadP.char ','
        y <- readPrec
        ReadPrec.lift $ ReadP.char ' '
        w <- readPrec
        ReadPrec.lift $ ReadP.char 'x'
        h <- readPrec
        pure Region{..}

instance Show Region where
    show Region{..} = mconcat [show x, ",", show y, " ", show w, "x", show h]
