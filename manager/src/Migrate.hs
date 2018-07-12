{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Migrate where

import Control.Monad
import System.Directory
import System.FilePath.Posix (isExtensionOf)

import qualified Database.PostgreSQL.Simple as PG

import Hash

-- | Return the list of available migration SQL files on the
-- migrations directory.
availableMigrations :: IO [FilePath]
availableMigrations =
  filter migrationFile <$> getDirectoryContents "manager/Migrations"
  where migrationFile = (".sql" `isExtensionOf`)


connection :: IO PG.Connection
connection = PG.connectPostgreSQL "postgres://aiplay:aiplay@localhost/aiplay"

-- | Return migrations that have been applied to the current database
appliedMigrations :: IO [(FilePath, Hash)]
appliedMigrations = do
  conn <- connection
  PG.query_ conn "SELECT filename, sha1sum FROM migrations"

initializeDatabase :: IO ()
initializeDatabase = do
  conn <- connection
  void $ PG.execute_ conn " \
\ DROP TABLE IF EXISTS migrations;                      \
\ CREATE TABLE IF NOT EXISTS migrations (               \
\   id serial PRIMARY KEY,                              \
\   sha1sum bytea NOT NULL UNIQUE,                      \
\   filename varchar(128) NOT NULL,                     \
\   applied_at timestamptz NOT NULL default now()       \
\ )"

insertMigrationIntoTable :: String -> Hash -> IO ()
insertMigrationIntoTable name sha1sum = do
  conn <- connection
  void $ PG.execute conn "INSERT INTO migrations (filename, sha1sum) VALUES (?, ?)" (name, sha1sum)

test2 :: IO ()
test2 = do
  conn <- connection
  void $ PG.execute conn "INSERT INTO migrations (filename, sha1sum) VALUES (?, ?)" ("foobar.txt" :: String, "1234" :: String)

test :: IO ()
test = do
  hash <- shaFile "manager/Migrations/20180810T154310-initial-schema.sql"
  insertMigrationIntoTable "filename1.txt" hash


-- | Run migrations against the database to ensure it is up-to-date
migrate :: IO ()
migrate = initializeDatabase
