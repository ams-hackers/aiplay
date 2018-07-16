module AIPlayTron.Protocol where

import Data.Char (toUpper)
import Data.List
import Text.Read

data Command =
  Move Int
       Int
  deriving (Show)

type Coord = (Int, Int)

data Result
  = Welcome Int
  | Turn
  | Size Int
         Int
  | Player Int
           Coord
  | Wall Coord
  | You Coord

parseCommand :: String -> Maybe Command
parseCommand = mkCommand . map (map toUpper) . words
  where
    mkCommand ["MOVE", x, y] = Move <$> readMaybe x <*> readMaybe y
    mkCommand _ = Nothing

formatResult :: Result -> String
formatResult (Welcome version) = unwords ["TRON", show version]
formatResult (Player playerId (x, y)) =
  unwords ["PLAYER", show playerId, show x, show y]
formatResult (Wall (x, y)) = unwords ["WALL", show x, show y]
formatResult (You (x, y)) = unwords ["YOU", show x, show y]
formatResult (Size x y) = unwords ["SIZE", show x, show y]
formatResult Turn = "TURN"
