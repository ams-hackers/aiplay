module Game (Game(..), Move(..), PlayerState(..), FinishedGame, Turn, play, updateGame) where

import Data.List

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Map.Lazy as Map
import Data.Map.Lazy (Map, (!?))

type Coord = (Int, Int)
type Player = Int

data PlayerState = Alive Coord | Dead
  deriving (Show, Eq)

alivePlayerState :: PlayerState -> Bool
alivePlayerState (Alive _) = True
alivePlayerState Dead = False

data Game = Game {
  gameTaken :: Set Coord,
  gamePlayers :: Map Player PlayerState
} deriving (Show)

listPlayers :: Game -> [Player]
listPlayers game = Map.keys $ gamePlayers game

listAlivePlayers :: Game -> [Player]
listAlivePlayers game = Map.keys $ Map.filter alivePlayerState $ gamePlayers game

data FinishedGame = FinishedGame {
  finishedGame :: Game,
  finishedGameWinners :: [Player]
}

emptyGame :: Game
emptyGame = Game {
  gameTaken = Set.empty,
  gamePlayers = Map.fromList [
    (1, Alive (10,10)),
    (2, Alive (5,5)),
    (3, Dead)
  ]
}


data Move = MoveUp | MoveDown | MoveLeft | MoveRight
  deriving (Show, Eq)

type Turn = Map Player Move

movePlayer :: PlayerState -> Move -> Maybe Coord
movePlayer (Alive (x,y)) MoveUp    = Just (  x, y-1)
movePlayer (Alive (x,y)) MoveDown  = Just (  x, y+1)
movePlayer (Alive (x,y)) MoveLeft  = Just (x-1, y)
movePlayer (Alive (x,y)) MoveRight = Just (x+1, y)
movePlayer Dead _                  = Nothing

-- Return a map of where each player would like to move in this turn
turnRequestedCoords :: Game -> Turn -> Map Player Coord
turnRequestedCoords game turn = Map.mapMaybeWithKey playerCoord (gamePlayers game)
  where playerCoord player playerState = do
          move <- turn !? player
          movePlayer playerState move

getNewPlayerState :: Player -> Set Coord -> Map Player Coord -> PlayerState
getNewPlayerState player taken requestedCoords =
  case Map.lookup player requestedCoords of
    Just targetCoord ->
      if isColliding targetCoord then Dead else Alive targetCoord
    Nothing -> Dead

  where otherPlayersCoords = Set.fromList $ Map.elems $ Map.delete player requestedCoords
        invalidCoords = taken `Set.union` otherPlayersCoords
        isColliding coords = coords `Set.member` invalidCoords

updateGame :: Game -> Turn -> Game
updateGame game turn = 
  Game {
      gameTaken = Set.union taken $ Set.fromList $ Map.elems requestedCoords,
      gamePlayers = Map.fromList [ (player, newPlayerState player) | player <- players ]
  }
  where players = listPlayers game
        taken = gameTaken game
        requestedCoords = turnRequestedCoords game turn
        newPlayerState player = getNewPlayerState player taken requestedCoords


winners :: Game -> Game -> Maybe [Player]
winners beforeGame afterGame =
  case aliveAfter of
    []       -> Just aliveBefore
    [player] -> Just [player]
    _        -> Nothing  
  where
    aliveBefore = listAlivePlayers beforeGame
    aliveAfter  = listAlivePlayers afterGame


play :: Game -> Turn -> Either FinishedGame Game
play beforeGame turn = 
  case winners beforeGame afterGame of
    Just players -> Left FinishedGame {
      finishedGame = afterGame,
      finishedGameWinners = players
    }
    Nothing -> Right afterGame
 where afterGame = updateGame beforeGame turn
        
