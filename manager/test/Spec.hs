module Main where

import Test.Hspec
import Migrate

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

main :: IO ()
main =
  hspec $ do
    testMigrate
