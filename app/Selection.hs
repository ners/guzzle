{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Selection where

import Control.Concurrent.Extra (once)
import Control.Exception (SomeException, try)
import Control.Monad.Extra (fromMaybeM, mconcatMapM)
import Data.Either (fromRight)
import Data.Either.Extra (eitherToMaybe)
import Data.Foldable (toList)
import Data.Foldable.Extra (firstJustM)
import Data.List.NonEmpty (nonEmpty)
import Hyprctl qualified
import Item (Item)
import Item qualified
import Niri qualified
import Persistence (NamedRegion (..))
import Persistence qualified
import Region (Region)
import Region qualified
import Slurp qualified
import Swaymsg qualified
import Prelude

type Mode = NonEmpty Region.Kind

anything :: Mode
anything = Region.Window :| [Region.Output, Region.Area]

data AreaSelector
    = ByName Text
    | LastArea
    | NewArea

data SelectionArgs = SelectionArgs
    { mode :: Mode
    , areaSelector :: AreaSelector
    , every :: Bool
    , noHistory :: Bool
    }

validate :: SelectionArgs -> Either Text ()
validate SelectionArgs{..} =
    maybe (Right ()) Left $ listToMaybe [message | (True, message) <- conflicts]
  where
    byName = case areaSelector of ByName{} -> True; _ -> False
    lastArea' = case areaSelector of LastArea -> True; _ -> False
    conflicts =
        [ (noHistory && byName, "--no-history cannot be combined with --area-name")
        , (noHistory && lastArea', "--no-history cannot be combined with --last-area")
        , (every && byName, "--all cannot be combined with --area-name")
        , (every && lastArea', "--all cannot be combined with --last-area")
        ,
            ( noHistory && every && mode == Region.Area :| []
            , "--all areas needs saved areas, but --no-history is set"
            )
        ]

selection :: SelectionArgs -> IO (NonEmpty Item)
selection args
    | args.every = everyItem args
    | otherwise = pure <$> selectOne args

everyItem :: SelectionArgs -> IO (NonEmpty Item)
everyItem args =
    fromMaybeM (fail . emptyMessage $ args.mode) $
        nonEmpty <$> modeCandidates args

emptyMessage :: Mode -> String
emptyMessage (Region.Area :| []) = "No saved areas"
emptyMessage (Region.Window :| []) = "No visible windows"
emptyMessage (Region.Output :| []) = "No outputs"
emptyMessage (Region.Screen :| []) = "No screen"
emptyMessage _ = "Nothing to capture"

selectOne :: SelectionArgs -> IO Item
selectOne args = case args.areaSelector of
    LastArea ->
        Item.plain Region.Area "last"
            <$> fromMaybeM (fail "No last area saved") (lastArea args)
    ByName name -> do
        region <-
            Persistence.getNamedRegion name
                >>= maybe (selectNewNamedRegion args name) (pure . (.region))
        remember args region
        pure $ Item.plain Region.Area name region
    NewArea -> do
        item <- selectNewItem args
        remember args item.region
        pure item

selectNewNamedRegion :: SelectionArgs -> Text -> IO Region
selectNewNamedRegion args name = do
    region <- (.region) <$> selectNewItem args
    Persistence.insertNamedRegion NamedRegion{..}
    pure region

remember :: SelectionArgs -> Region -> IO ()
remember SelectionArgs{noHistory = True} _ = pure ()
remember _ region = Persistence.setLastRegion region

selectNewItem :: SelectionArgs -> IO Item
selectNewItem SelectionArgs{mode = Region.Screen :| []} =
    screen =<< getDesktop
selectNewItem SelectionArgs{mode = Region.Area :| [], areaSelector = ByName{}} =
    Item.plain Region.Area "" . fst <$> Slurp.pick [] []
selectNewItem args = do
    listed <- pickable args
    (region, pick) <- Slurp.pick (slurpFlags args listed) (fmap (.region) listed)
    pure $ case pick of
        Slurp.Listed index ->
            fromMaybe (drawn region) . listToMaybe $ drop index listed
        Slurp.Output name -> Item.outputNamed name region
        Slurp.Drawn -> drawn region
  where
    drawn = Item.plain Region.Area ""

slurpFlags :: SelectionArgs -> [Item] -> [Text]
slurpFlags args listed =
    ["-r" | Region.Area `notElem` args.mode]
        <> [ "-o" | Region.Output `elem` args.mode, all ((/= Region.Output) . (.kind)) listed
           ]

pickable :: SelectionArgs -> IO [Item]
pickable args = case args.mode of
    Region.Output :| [] -> bestEffort $ modeCandidates args
    _ -> modeCandidates args

modeCandidates :: SelectionArgs -> IO [Item]
modeCandidates args = do
    desktop <- once getDesktop
    case args.mode of
        kind :| [] -> candidates desktop args kind
        kinds -> mconcatMapM (bestEffort . candidates desktop args) (toList kinds)

candidates :: IO Item.Desktop -> SelectionArgs -> Region.Kind -> IO [Item]
candidates _ args Region.Area = areaCandidates args
candidates desktop _ Region.Window = (.windows) <$> desktop
candidates desktop _ Region.Output = (.outputs) <$> desktop
candidates desktop _ Region.Screen = pure <$> (screen =<< desktop)

screen :: Item.Desktop -> IO Item
screen Item.Desktop{outputs = []} = fail "Cannot get screen information"
screen Item.Desktop{outputs} =
    pure . Item.plain Region.Screen "screen" $ foldMap (.region) outputs

getDesktop :: IO Item.Desktop
getDesktop =
    firstSuccess
        "Cannot get window manager information"
        [ Swaymsg.getDesktop
        , Hyprctl.getDesktop
        , Niri.getDesktop
        ]

areaCandidates :: SelectionArgs -> IO [Item]
areaCandidates args = do
    named <- namedAreas args
    lastRegion <- lastArea args
    pure $
        (named <&> \NamedRegion{..} -> Item.plain Region.Area name region)
            <> ( Item.plain Region.Area "last"
                    <$> filter (`notElem` fmap (.region) named) (maybeToList lastRegion)
               )

namedAreas :: SelectionArgs -> IO [NamedRegion]
namedAreas SelectionArgs{noHistory = True} = pure []
namedAreas _ = Persistence.getAllNamedRegions

lastArea :: SelectionArgs -> IO (Maybe Region)
lastArea SelectionArgs{noHistory = True} = pure Nothing
lastArea _ = Persistence.getLastRegion

bestEffort :: IO [a] -> IO [a]
bestEffort = fmap (fromRight []) . try @SomeException

firstSuccess :: String -> [IO a] -> IO a
firstSuccess message =
    fromMaybeM (fail message) . firstJustM (fmap eitherToMaybe . try @SomeException)
