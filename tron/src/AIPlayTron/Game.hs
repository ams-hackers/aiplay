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

import Data.Map.Lazy (Map)
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

instance Show Game where
  show Game {gameTaken, gamePlayers} =
    "Game { taken=" ++
    show gameTaken ++ " , players=" ++ show gamePlayers ++ "}"

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
  } deriving (Show)

-- | A turn in the game. All user moves are to be considered
-- simultaneously.
type Turn = Map Player Coord

distance :: Coord -> Coord -> Int
distance (Coord (x, y)) (Coord (x', y')) = abs (x' - x) + abs (y' - y)

moveInvalidDistance :: Coord -> Coord -> Bool
moveInvalidDistance c1 c2 = distance c1 c2 /= 1

getNewPlayerState ::
     PlayerState -> Player -> Set Coord -> Map Player Coord -> PlayerState
getNewPlayerState Dead _ _ _ = Dead
getNewPlayerState (Alive sourceCoords) player taken requestedCoords =
  case Map.lookup player requestedCoords of
    Just targetCoord
      | isColliding targetCoord -> Dead
      | moveInvalidDistance sourceCoords targetCoord -> Dead
      | otherwise -> Alive targetCoord
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
  { gameTaken = Set.union taken $ Set.fromList $ Map.elems turn
  , gamePlayers =
      Map.fromList [(player, newPlayerState player) | player <- players]
  , gameHistory = gameHistory game ++ [turn]
  }
  where
    players = listPlayers game
    taken = gameTaken game
    newPlayerState player =
      getNewPlayerState (getPlayerState game player) player taken turn

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
