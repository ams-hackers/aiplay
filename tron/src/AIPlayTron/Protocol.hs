module AIPlayTron.Protocol where

import Data.Char (toUpper)
import Text.Read

data Command =
  Move Int
       Int
  deriving (Show)

type Coord = (Int, Int)

data Response
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

formatResponse :: Response -> String
formatResponse (Welcome version) = unwords ["TRON", show version]
formatResponse (Player playerId (x, y)) =
  unwords ["PLAYER", show playerId, show x, show y]
formatResponse (Wall (x, y)) = unwords ["WALL", show x, show y]
formatResponse (You (x, y)) = unwords ["YOU", show x, show y]
formatResponse (Size x y) = unwords ["SIZE", show x, show y]
formatResponse Turn = "TURN"
