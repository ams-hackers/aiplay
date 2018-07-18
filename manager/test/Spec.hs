module Main where

import Migrate
import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

main :: IO ()
main = hspec $ do testMigrate
