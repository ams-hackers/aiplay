{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

{-| Model the rules of the game Tron.
-}
module AIPlayTron.Game where

import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Data.Text as Text

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map.Lazy (Map, (!?))
import qualified Data.Map.Lazy as Map

-- A position in the board
newtype Coord =
  Coord (Int, Int)
  deriving (Show, Eq, Ord)

newtype Player =
  Player Int
  deriving (Show, Eq, Ord)

data PlayerState
  = Alive Coord
  | Dead
  deriving (Show, Eq)

-- | A Tron game. It captures the state of the game in a given turn.
data Game = Game
  { gameTaken :: Set Coord
  , gamePlayers :: Map Player PlayerState
  , gameInitial :: Game
  , gameHistory :: [Turn]
  }

-- * Construction
-- | Return an empty game
emptyGame :: Game
emptyGame =
  Game
  { gameTaken = Set.empty
  , gamePlayers =
      Map.fromList
        [ (Player 1, Alive $ Coord (10, 10))
        , (Player 2, Alive $ Coord (5, 5))
        , (Player 3, Dead)
        ]
  , gameInitial = emptyGame
  , gameHistory = []
  }

-- * Basic operations
alivePlayerState :: PlayerState -> Bool
alivePlayerState (Alive _) = True
alivePlayerState Dead = False

getPlayerState :: Game -> Player -> PlayerState
getPlayerState game player =
  fromMaybe Dead (Map.lookup player $ gamePlayers game)

-- | The players participating in this game.
listPlayers :: Game -> [Player]
listPlayers game = Map.keys $ gamePlayers game

-- | The players that are alive.
listAlivePlayers :: Game -> [Player]
listAlivePlayers game =
  Map.keys $ Map.filter alivePlayerState $ gamePlayers game

-- * Playing
-- This is some tests for this section.
--
-- | A finished game represents the result of a game. Note that you
-- cannot 'play' more turns on a finished game.
data FinishedGame = FinishedGame
  { finishedGame :: Game
  , finishedGameWinners :: [Player]
  }

-- | A move is a possible action that a player can take
data Move
  = MoveUp
  | MoveDown
  | MoveLeft
  | MoveRight
  deriving (Show, Eq)

-- | A turn in the game. All user moves are to be considered
-- simultaneously.
type Turn = Map Player Move

movePlayer :: PlayerState -> Move -> Maybe Coord
movePlayer (Alive (Coord (x, y))) MoveUp = Just $ Coord (x, y - 1)
movePlayer (Alive (Coord (x, y))) MoveDown = Just $ Coord (x, y + 1)
movePlayer (Alive (Coord (x, y))) MoveLeft = Just $ Coord (x - 1, y)
movePlayer (Alive (Coord (x, y))) MoveRight = Just $ Coord (x + 1, y)
movePlayer Dead _ = Nothing

-- Return a map of where each player would like to move in this turn
turnRequestedCoords :: Game -> Turn -> Map Player Coord
turnRequestedCoords game turn =
  Map.mapMaybeWithKey playerCoord (gamePlayers game)
  where
    playerCoord player playerState = do
      move <- turn !? player
      movePlayer playerState move

getNewPlayerState :: Player -> Set Coord -> Map Player Coord -> PlayerState
getNewPlayerState player taken requestedCoords =
  case Map.lookup player requestedCoords of
    Just targetCoord ->
      if isColliding targetCoord
        then Dead
        else Alive targetCoord
    Nothing -> Dead
  where
    otherPlayersCoords =
      Set.fromList $ Map.elems $ Map.delete player requestedCoords
    invalidCoords = taken `Set.union` otherPlayersCoords
    isColliding coords = coords `Set.member` invalidCoords

-- Get the resulting game after playing a turn.
updateGame :: Game -> Turn -> Game
updateGame game turn =
  game
  { gameTaken = Set.union taken $ Set.fromList $ Map.elems requestedCoords
  , gamePlayers =
      Map.fromList [(player, newPlayerState player) | player <- players]
  , gameHistory = gameHistory game ++ [turn]
  }
  where
    players = listPlayers game
    taken = gameTaken game
    requestedCoords = turnRequestedCoords game turn
    newPlayerState player = getNewPlayerState player taken requestedCoords

-- Return an array of winners. There could be multiple winners if
-- there is a tie. It will return Nothing if the game is not finished
-- yet.
winners :: Game -> Game -> Maybe [Player]
winners beforeGame afterGame =
  case aliveAfter of
    [] -> Just aliveBefore
    [player] -> Just [player]
    _ -> Nothing
  where
    aliveBefore = listAlivePlayers beforeGame
    aliveAfter = listAlivePlayers afterGame

-- | Play a turn on a given game, returning either a Game or a
-- FinishedGame.
play :: Game -> Turn -> Either FinishedGame Game
play beforeGame turn =
  case winners beforeGame afterGame of
    Just players ->
      Left
        FinishedGame {finishedGame = afterGame, finishedGameWinners = players}
    Nothing -> Right afterGame
  where
    afterGame = updateGame beforeGame turn

--
-- JSON Serialization
--
instance ToJSON Coord where
  toJSON (Coord c) = toJSON c

instance ToJSON Player where
  toJSON (Player i) = toJSON i

instance ToJSONKey Player where
  toJSONKey = toJSONKeyText $ \(Player i) -> Text.pack $ show i

instance ToJSON Move where
  toJSON m =
    case m of
      MoveUp -> "up"
      MoveDown -> "down"
      MoveLeft -> "left"
      MoveRight -> "right"

instance ToJSON FinishedGame where
  toJSON FinishedGame {finishedGame, finishedGameWinners} =
    object
      [ "walls" .= gameTaken initial
      , "startPositions" .= Map.mapMaybe playerStateCoord (gamePlayers initial)
      , "turns" .= gameHistory finishedGame
      , "winners" .= finishedGameWinners
      ]
    where
      initial = gameInitial finishedGame
      playerStateCoord (Alive c) = Just c
      playerStateCoord Dead = Nothing
