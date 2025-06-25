module Slurp where

import Data.Text qualified as Text
import Region
import Prelude

slurp :: [Text] -> Text -> IO Region
slurp p t = read . fromText <$> textCmd ("slurp" :| "-d" : p) (textInput t)

selectNewOrExistingRegion :: [Region] -> IO Region
selectNewOrExistingRegion = slurp [] . Text.unlines . fmap ishow

selectNewRegion :: IO Region
selectNewRegion = slurp [] ""

selectRegion :: [Region] -> IO Region
selectRegion = slurp ["-r"] . Text.unlines . fmap ishow

selectOutput :: IO Region
selectOutput = slurp ["-o", "-r"] ""

selectAnything :: [Region] -> IO Region
selectAnything = slurp ["-o"] . Text.unlines . fmap ishow
