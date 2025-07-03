module WlCopy where

import Content
import System.Directory (canonicalizePath)
import System.Process.Typed qualified as Process
import Prelude

wlCopy :: Content -> IO ()
wlCopy Content{..} = cmd_ ["wl-copy"] $ Process.byteStringInput content

wlCopyFile :: FilePath -> IO ()
wlCopyFile file = do
    realfile <- canonicalizePath file
    cmd_
        ["wl-copy", "--type=text/uri-list", "file:" <> fromString realfile]
        nullStream
