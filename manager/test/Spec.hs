{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.QuickCheck

import Data.ByteString.Lazy.Char8 (pack)

import AIPlayManager.Migrate
import AIPlayManager.SHA1

fakeMigration :: String -> Migration a
fakeMigration x = Migration {filename = x, sha1sum = sha1LBS $ pack x}

main :: IO ()
main =
  hspec $ do
    describe "Migrate" $ do
      it "should consider an empty database and project as up-top-date" $ do
        let migrations = []
            -- Define new variables for them so we don't constraint
            -- the polymorphism type of migrations.
            fileMigrations = migrations
            recordedMigrations = migrations
            status = compareMigrations fileMigrations recordedMigrations
        migrationStatusPending status `shouldBe` []
        migrationStatusMissing status `shouldBe` []
        migrationStatusChanged status `shouldBe` []
        migrationStatusApplied status `shouldBe` migrations
      it "should not do anything if the database is up-to-date" $ do
        let migrations =
              [ fakeMigration "1.sql"
              , fakeMigration "2.sql"
              , fakeMigration "3.sql"
              ]
            fileMigrations = migrations
            recordedMigrations = migrations
            status = compareMigrations fileMigrations recordedMigrations
        migrationStatusPending status `shouldBe` []
        migrationStatusMissing status `shouldBe` []
        migrationStatusChanged status `shouldBe` []
        migrationStatusApplied status `shouldBe` migrations
      it "should apply new migration scripts!" $ do
        let migrations =
              [ fakeMigration "1.sql"
              , fakeMigration "2.sql"
              , fakeMigration "3.sql"
              ]
            newMigration = fakeMigration "4.sql"
            fileMigrations = newMigration : migrations
            recordedMigrations = migrations
            status = compareMigrations fileMigrations recordedMigrations
        migrationStatusPending status `shouldBe` [newMigration]
        migrationStatusMissing status `shouldBe` []
        migrationStatusChanged status `shouldBe` []
        migrationStatusApplied status `shouldBe` migrations
      it "should complain about missing migrations" $ do
        let migrations =
              [ fakeMigration "1.sql"
              , fakeMigration "2.sql"
              , fakeMigration "3.sql"
              ]
            missingMigration = fakeMigration "4.sql"
            fileMigrations = migrations
            recordedMigrations = missingMigration : migrations
            status = compareMigrations fileMigrations recordedMigrations
        migrationStatusPending status `shouldBe` []
        migrationStatusMissing status `shouldBe` [missingMigration]
        migrationStatusChanged status `shouldBe` []
        migrationStatusApplied status `shouldBe` migrations
      it "should complain about changed migrations" $ do
        let fileMigration =
              Migration {filename = "test", sha1sum = sha1LBS "test-changed"}
            recordedMigration =
              Migration {filename = "test", sha1sum = sha1LBS "test"}
            status = compareMigrations [fileMigration] [recordedMigration]
        migrationStatusPending status `shouldBe` []
        migrationStatusMissing status `shouldBe` []
        migrationStatusChanged status `shouldBe`
          [(fileMigration, recordedMigration)]
        migrationStatusApplied status `shouldBe` []
