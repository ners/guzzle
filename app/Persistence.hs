{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Persistence where

import Data.Hashable
import Data.Int (Int64)
import Database.SQLite.Simple
    ( Connection
    , FromRow
    , NamedParam ((:=))
    , ToRow
    , fromRow
    , toRow
    )
import Database.SQLite.Simple qualified as SQLite.Simple
import Database.SQLite.Simple.FromRow (field)
import Database.SQLite.Simple.QQ
import Database.SQLite.Simple.ToField (ToField (toField))
import Region
import System.Directory
    ( XdgDirectory (..)
    , createDirectoryIfMissing
    , getXdgDirectory
    )
import System.FilePath ((</>))
import Prelude

data NamedRegion = NamedRegion
    { name :: Text
    , region :: Region
    }
    deriving stock (Eq, Generic)

deriving anyclass instance FromRow Region

deriving anyclass instance ToRow Region

instance FromRow NamedRegion where
    fromRow = do
        name <- field
        region <- fromRow
        pure NamedRegion{..}

instance ToRow NamedRegion where
    toRow NamedRegion{..} = toField name : toRow region

deriving anyclass instance Hashable Region

deriving anyclass instance Hashable NamedRegion

createNamedRegionTable :: IO ()
createNamedRegionTable =
    void . execute' $
        [sql|
            CREATE TABLE IF NOT EXISTS regions
                ( name TEXT NOT NULL
                , x INTEGER NOT NULL
                , y INTEGER NOT NULL
                , w INTEGER NOT NULL
                , h INTEGER NOT NULL
                , PRIMARY KEY (name)
                )
        |]

getAllRegions :: IO [NamedRegion]
getAllRegions = queryNamed [sql| SELECT * FROM regions |] []

getRegionByName :: Text -> IO (Maybe NamedRegion)
getRegionByName name =
    listToMaybe
        <$> queryNamed
            [sql| SELECT * FROM regions WHERE name = :name |]
            [":name" := name]

insertRegion :: NamedRegion -> IO ()
insertRegion namedRegion =
    execute_
        [sql|
            INSERT OR REPLACE INTO regions (name, x, y, w, h) VALUES (?, ?, ?, ?, ?)
        |]
        namedRegion

deleteRegionByName :: Text -> IO Bool
deleteRegionByName name =
    executeNamed
        [sql| DELETE FROM regions WHERE name = :name |]
        [":name" := name]
        <&> (> 0)

withDb :: (Connection -> IO a) -> IO a
withDb f = do
    dir <- getXdgDirectory XdgState "guzzle"
    createDirectoryIfMissing True dir
    let dbFile = dir </> "guzzle.db"
    SQLite.Simple.withConnection dbFile f

queryNamed
    :: (SQLite.Simple.FromRow r) => SQLite.Simple.Query -> [NamedParam] -> IO [r]
queryNamed q ps = withDb \c -> SQLite.Simple.queryNamed c q ps

execute_ :: (SQLite.Simple.ToRow p) => SQLite.Simple.Query -> p -> IO ()
execute_ q p = withDb \c -> SQLite.Simple.execute c q p

-- | Runs the query and returns the number of affected rows.
executeNamed :: SQLite.Simple.Query -> [NamedParam] -> IO Int
executeNamed q ps = withDb \c -> do
    SQLite.Simple.executeNamed c q ps
    SQLite.Simple.changes c

execute' :: SQLite.Simple.Query -> IO Int
execute' = flip executeNamed []

-- | Runs the query and returns the ID of the last inserted row.
executeWithLastRowId
    :: (SQLite.Simple.ToRow p) => SQLite.Simple.Query -> p -> IO Int64
executeWithLastRowId q p = withDb \c -> do
    SQLite.Simple.execute c q p
    SQLite.Simple.lastInsertRowId c
