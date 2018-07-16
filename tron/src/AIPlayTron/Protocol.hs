module AIPlayTron.Protocol where

import Data.Char (toUpper)
import Text.Read

data Command =
  Move Int
       Int
  deriving (Show)

parseCommand :: String -> Maybe Command
parseCommand = mkCommand . (map (map toUpper)) . words
  where
    mkCommand ["MOVE", x, y] = Move <$> readMaybe x <*> readMaybe y
    mkCommand _ = Nothing
