import Test.Hspec
import Test.QuickCheck

import qualified Data.Set as Set

import AIPlayTron.Game

{-# ANN module "HLint: ignore Redundant do" #-}

{-# ANN module "HLint: ignore Redundant $" #-}

instance Arbitrary Coord where
  arbitrary = do
    x <- choose (0, 100)
    y <- choose (0, 100)
    return $ Coord (x, y)

instance Arbitrary Player where
  arbitrary = Player <$> choose (0, 10)

instance Arbitrary PlayerState where
  arbitrary =
    oneof
      [ do coord <- arbitrary
           return $ Alive coord
      , return Dead
      ]

instance Arbitrary Move where
  arbitrary = elements [MoveUp, MoveDown, MoveLeft, MoveRight]

instance Arbitrary Game where
  arbitrary = do
    taken <- arbitrary
    players <- arbitrary
    return $ Game {gameTaken = taken, gamePlayers = players, gameHistory = []}

monotoneTaken :: Game -> Turn -> Bool
monotoneTaken game turn = countTaken game <= countTaken game'
  where
    game' = updateGame game turn
    countTaken = Set.size . gameTaken

main :: IO ()
main =
  hspec $ do
    describe "Game" $ do
      it "The number of taken cell will not decrease" $ property monotoneTaken
