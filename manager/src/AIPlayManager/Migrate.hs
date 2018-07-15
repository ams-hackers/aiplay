{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune #-}

{-|
This module implements simple database migrations by running all the
scripts a directory and tracking when they have been applied, so they
are not applied again.
-}
module AIPlayManager.Migrate where

import Control.Monad
import Data.Function
import Data.List
import System.Directory
import System.FilePath.Posix ((</>), isExtensionOf, takeFileName)

import AIPlayManager.SHA1
import qualified Data.ByteString as BS
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.FromRow as FromRow

data Recorded

data File

-- | A database migration represents a SQL script that can be or has
-- been applied to the database.
data Migration a = Migration
  { filename :: FilePath
  , sha1sum :: SHA1
  } deriving (Show)

instance Eq (Migration a) where
  (==) = on (==) filename

instance Ord (Migration a) where
  (<=) = on (<=) filename

cast :: Migration a -> Migration b
cast Migration {filename, sha1sum} = Migration {filename, sha1sum}

castAll :: [Migration a] -> [Migration b]
castAll = map cast

-- Check if two migrations are equal and they have the same sha1sum.
consistentlyEqual :: Migration a -> Migration b -> Bool
consistentlyEqual m1 m2 = m1 == cast m2 && sha1sum m1 == sha1sum m2

migrationdir :: FilePath
migrationdir = "./Migrations"

readMigrationFile :: Migration File -> IO BS.ByteString
readMigrationFile m = BS.readFile $ migrationdir </> filename m

migrationFromFile :: FilePath -> IO (Migration File)
migrationFromFile file = do
  let filename = takeFileName file
  sha1sum <- sha1File $ migrationdir </> file
  return Migration {filename, sha1sum}

listMigrationFiles :: IO [FilePath]
listMigrationFiles = filter isMigrationFile <$> listDirectory "./Migrations"
  where
    isMigrationFile = (".sql" `isExtensionOf`)

-- | Return the list of available migration SQL files on the
-- migrations directory.
availableMigrations :: IO [Migration File]
availableMigrations = sort <$> listMigrationFiles >>= mapM migrationFromFile

--
-- Database
--
connection :: IO PG.Connection
connection = PG.connectPostgreSQL "postgres://aiplay:aiplay@localhost/aiplay"

-- Ensure that the migrations table has been created in the database
initializeDatabase :: IO ()
initializeDatabase = do
  conn <- connection
  void $
    PG.execute_
      conn
      " \
\ CREATE TABLE IF NOT EXISTS migrations (               \
\   id serial PRIMARY KEY,                              \
\   sha1sum bytea NOT NULL UNIQUE,                      \
\   filename varchar(128) NOT NULL,                     \
\   applied_at timestamptz NOT NULL default now()       \
\ )"

-- | Return migrations that have been applied to the current database.
appliedMigrations :: IO [Migration Recorded]
appliedMigrations = do
  conn <- connection
  PG.queryWith_
    migrationResult
    conn
    "SELECT filename, sha1sum FROM migrations order by id"
  where
    migrationResult = do
      filename <- FromRow.field
      sha1sum <- FromRow.field :: FromRow.RowParser SHA1
      return Migration {filename, sha1sum}

-- Mark a migration as applied
insertMigrationIntoTable :: Migration File -> IO ()
insertMigrationIntoTable Migration {filename, sha1sum} = do
  conn <- connection
  void $
    PG.execute
      conn
      "INSERT INTO migrations (filename, sha1sum) VALUES (?, ?)"
      (filename, sha1sum)

--
-- Migration
--
-- | The status of a 'Migration'
data MigrationStatus = MigrationStatus
  { migrationStatusPending :: [Migration File]
  , migrationStatusApplied :: [Migration File]
  , migrationStatusChanged :: [(Migration File, Migration Recorded)]
  , migrationStatusMissing :: [Migration Recorded]
  } deriving (Show)

-- Pair up avaiable and applied migrations as a preparation for a
-- consistency check.
matchingMigrations ::
     [Migration File]
  -> [Migration Recorded]
  -> [(Migration File, Migration Recorded)]
matchingMigrations available applied =
  zip
    (sort $ available `intersect` castAll applied)
    (sort $ applied `intersect` castAll applied)

compareMigrations :: [Migration File] -> [Migration Recorded] -> MigrationStatus
compareMigrations available applied =
  MigrationStatus
  { migrationStatusPending = available \\ castAll applied
  , migrationStatusMissing = applied \\ castAll available
  , migrationStatusApplied =
      [m1 | (m1, m2) <- matching, m1 `consistentlyEqual` m2]
  , migrationStatusChanged = filter (not . uncurry consistentlyEqual) matching
  }
  where
    matching = matchingMigrations available applied

-- | Run migrations against the database to ensure it is up-to-date.
migrate :: IO ()
migrate = initializeDatabase
