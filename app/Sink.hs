module Sink where

import Content
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Foldable (for_)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Data.Traversable (for)
import Item (Item)
import Item qualified
import Notify qualified
import System.Directory (canonicalizePath, createDirectoryIfMissing)
import System.FilePath (takeDirectory, (-<.>))
import Template (Template)
import Template qualified
import WlCopy qualified
import Prelude

data SinkAction
    = Copy
    | Save
    | Print
    deriving stock (Eq)

data SinkArgs = SinkArgs
    { sinkAction :: Maybe SinkAction
    , file :: Maybe Template
    , noNotify :: Bool
    }

sink :: SinkArgs -> NonEmpty (Item, Content) -> IO ()
sink SinkArgs{..} results = do
    template <-
        maybe (either fatalError pure (Template.parse "guzzle-%d")) pure file
    savedPaths <-
        if hasFile
            then Just <$> saveFiles template results
            else pure Nothing
    case action of
        Copy -> case savedPaths of
            Just paths -> do
                WlCopy.wlCopyFiles paths
                notifySaved "saved and copied" paths
            Nothing -> do
                WlCopy.wlCopy $ snd firstResult
                notify (noun <> " copied to clipboard") ""
        Save -> for_ savedPaths $ notifySaved "saved"
        Print -> for_ results $ LazyByteString.putStr . content . snd
  where
    action = fromMaybe Copy sinkAction
    firstResult = NonEmpty.head results
    video = isVideo . contentType $ snd firstResult
    noun = if video then "Video" else "Screenshot"
    hasFile =
        isJust file
            || sinkAction == Just Save
            || video
            || (length results > 1 && action == Copy)
    notify title = unless noNotify . Notify.notify title
    notifySaved verb paths =
        notify (summary verb) . Text.pack $ case paths of
            path :| [] -> path
            path :| _ -> takeDirectory path
    summary verb = case results of
        _ :| [] -> noun <> " " <> verb
        _ -> ishow (length results) <> " " <> Text.toLower noun <> "s " <> verb

saveFiles :: Template -> NonEmpty (Item, Content) -> IO (NonEmpty FilePath)
saveFiles template results = do
    time <- Item.timestamp
    let names =
            NonEmpty.zip (1 :| [2 ..]) results <&> \(index, (item, Content{..})) ->
                ensureExtension template contentType $
                    Template.render (Item.placeholder time index item) template
    for (NonEmpty.zip (uniqueNames names) results) \(name, (_, Content{..})) -> do
        createDirectoryIfMissing True $ takeDirectory name
        path <- canonicalizePath name
        LazyByteString.writeFile path content
        printInfo $ "Saved file " <> Text.pack path
        pure path
  where
    uniqueNames = NonEmpty.fromList . Template.unique . NonEmpty.toList

ensureExtension :: Template -> ContentType -> FilePath -> FilePath
ensureExtension template contentType path
    | isJust (Template.extension template) = path -<.> extension contentType
    | otherwise = path <> "." <> extension contentType
