{-# ANN module "HLint: ignore Redundant do" #-}

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import qualified Data.Set as Set

import Game (Game(..), Move(..), PlayerState(..), updateGame, Turn)

instance Arbitrary PlayerState where
  arbitrary = oneof [
    do
      x <- arbitrary
      y <- arbitrary
      return $ Alive (x,y)
    ,
      return Dead
    ]

instance Arbitrary Move where
  arbitrary = elements [MoveUp, MoveDown, MoveLeft, MoveRight]


instance Arbitrary Game where
  arbitrary = Game <$> arbitrary <*> arbitrary

monotoneTaken :: Game -> Turn -> Bool
monotoneTaken game turn = countTaken game <= countTaken game'
  where game' = updateGame game turn
        countTaken = Set.size . gameTaken

main :: IO ()
main = hspec $ do
  describe "Game" $ do

    it "The number of taken cell will not decrease" $
      property monotoneTaken
