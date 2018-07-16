{-# LANGUAGE OverloadedStrings #-}

module AIPlayTron.Protocol where

import qualified Data.Text as T
import qualified Data.Text.Read as Read

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

numArg :: T.Text -> Maybe Int
numArg x =
  case Read.decimal x of
    Left _ -> Nothing
    Right (value, _) -> Just value

wordsToCommand :: [T.Text] -> Maybe Command
wordsToCommand ["MOVE", x, y] = Move <$> numArg x <*> numArg y
wordsToCommand _ = Nothing

parseCommand :: T.Text -> Maybe Command
parseCommand = wordsToCommand . map T.toUpper . T.words

str :: Show a => a -> T.Text
str = T.pack . show

formatResponse :: Response -> T.Text
formatResponse (Welcome version) = T.unwords ["TRON", str version]
formatResponse (Player playerId (x, y)) =
  T.unwords ["PLAYER", str playerId, str x, str y]
formatResponse (Wall (x, y)) = T.unwords ["WALL", str x, str y]
formatResponse (You (x, y)) = T.unwords ["YOU", str x, str y]
formatResponse (Size x y) = T.unwords ["SIZE", str x, str y]
formatResponse Turn = "TURN"
