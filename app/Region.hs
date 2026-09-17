module Region where

import Text.ParserCombinators.ReadP qualified as ReadP
import Text.ParserCombinators.ReadPrec qualified as ReadPrec
import Text.Read
import Prelude

data Region = Region
    { x :: Int
    , y :: Int
    , w :: Int
    , h :: Int
    }
    deriving stock (Eq, Generic)

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

-- | The union of two regions: the smallest region that contains both.
instance Semigroup Region where
    a <> b = Region{x = x0, y = y0, w = x1 - x0, h = y1 - y0}
      where
        x0 = min (x a) (x b)
        y0 = min (y a) (y b)
        x1 = max (x a + w a) (x b + w b)
        y1 = max (y a + h a) (y b + h b)

-- | The empty region at 0,0
instance Monoid Region where
    mempty = Region{x = 0, y = 0, w = 0, h = 0}
