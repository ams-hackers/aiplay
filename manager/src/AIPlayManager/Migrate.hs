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
import Data.String
import System.Directory
import System.Exit
import System.FilePath.Posix ((</>), isExtensionOf, takeFileName)
import System.IO

import AIPlayManager.SHA1
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS.Char8
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.FromRow as FromRow

data Record

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
\ SET client_min_messages TO WARNING;                   \
\ CREATE TABLE IF NOT EXISTS migrations (               \
\   id serial PRIMARY KEY,                              \
\   sha1sum bytea NOT NULL UNIQUE,                      \
\   filename varchar(128) NOT NULL,                     \
\   applied_at timestamptz NOT NULL default now()       \
\ )"

-- | Return migrations that have been applied to the current database.
appliedMigrations :: IO [Migration Record]
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

runMigration :: Migration File -> IO ()
runMigration m = do
  content <- readMigrationFile m
  conn <- connection
  PG.withTransaction conn $ do
    void $ PG.execute_ conn $ fromString $ BS.Char8.unpack content
    insertMigrationIntoTable m

--
-- Migration
--
-- | The status of a 'Migration'
data MigrationStatus = MigrationStatus
  { migrationStatusPending :: [Migration File]
  , migrationStatusApplied :: [Migration File]
  , migrationStatusChanged :: [(Migration File, Migration Record)]
  , migrationStatusMissing :: [Migration Record]
  } deriving (Show)

-- Pair up available and applied migrations as a preparation for a
-- consistency check.
matchingMigrations ::
     [Migration File]
  -> [Migration Record]
  -> [(Migration File, Migration Record)]
matchingMigrations available applied =
  zip
    (sort $ available `intersect` castAll applied)
    (sort $ applied `intersect` castAll applied)

compareMigrations :: [Migration File] -> [Migration Record] -> MigrationStatus
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

printFailedMigrations :: [Migration a] -> IO ()
printFailedMigrations =
  mapM_ (\migration -> do hPutStrLn stderr $ " - " ++ (filename migration))

-- | Run migrations against the database to ensure it is up-to-date.
migrate :: IO ()
migrate = do
  initializeDatabase
  available <- availableMigrations
  applied <- appliedMigrations
  let status = compareMigrations available applied
  case status of
    _
      | migrationStatusChanged status /= [] -> do
        hPutStrLn
          stderr
          "ERROR: The following migrations files changed the directory:"
        printFailedMigrations $ map fst $ migrationStatusChanged status
        void exitFailure
      | migrationStatusMissing status /= [] -> do
        hPutStrLn
          stderr
          "ERROR: The following migrations were applied to the database but are missing from the directory:"
        printFailedMigrations $ migrationStatusMissing status
        void exitFailure
      | migrationStatusPending status == [] -> do
        putStrLn "Database is up-to-date."
        void exitSuccess
      | otherwise -> do
        let migrations = migrationStatusPending status
        forM_ migrations $ \m -> do
          putStrLn $ "Running " ++ filename m
          runMigration m
        void exitSuccess
