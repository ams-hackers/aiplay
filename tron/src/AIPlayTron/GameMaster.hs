{-# LANGUAGE OverloadedStrings #-}

module AIPlayTron.GameMaster where

import qualified AIPlayTron.Game as Game
import AIPlayTron.Protocol
import AIPlayTron.Socket
import Control.Exception (finally)
import Data.Char (isSpace)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import Network.Socket
import System.IO

port :: PortNumber
port = 4242

numberOfPlayers :: Int
numberOfPlayers = 2

main :: IO ()
main = do
  sock <- listenPlayers numberOfPlayers port
  putStrLn $ "Listening on " ++ show port
  finally
    (do conns <- acceptPlayers sock numberOfPlayers
        close sock
        runGame conns)
    (close sock)

readLine :: Handle -> IO T.Text
readLine handle = T.dropWhileEnd isSpace <$> IO.hGetLine handle

readCommand :: Handle -> IO (Maybe Command)
readCommand handle = parseCommand <$> readLine handle

reply :: Response -> Handle -> IO ()
reply response handle = IO.hPutStrLn handle $ formatResponse response

replyAll :: Response -> [Handle] -> IO ()
replyAll response = mapM_ (reply response)

sendHello :: Handle -> IO ()
sendHello handle = do
  msg $ Welcome 1
  msg $ Size 100 100
  msg $ Wall (1, 1)
  msg $ Wall (2, 3)
  msg $ Player 1 (10, 10)
  msg $ You (1, 2)
  where
    msg = (`reply` handle)

getMove :: Game.PlayerState -> Maybe Command -> Maybe Game.Move
getMove (Game.Alive (Game.Coord (x, y))) (Just (Move x' y')) =
  d (x' - x) (y' - y)
  where
    d 1 0 = Just Game.MoveRight
    d (-1) 0 = Just Game.MoveLeft
    d 0 1 = Just Game.MoveDown
    d 0 (-1) = Just Game.MoveUp
    d _ _ = Nothing
getMove _ _ = Nothing

-- test2 = getMove (Game.Alive (Game.Coord (5, 5))) (Just $ Move 6 5)
getTurn :: Game.Game -> [(Game.Player, Maybe Command)] -> Game.Turn
getTurn game responses =
  Map.fromList [(player, move) | (player, Just move) <- currentStates]
  where
    currentStates =
      [ (player, getMove (Game.getPlayerState game player) command)
      | (player, command) <- responses
      ]

-- test = getTurn Game.emptyGame [(Game.Player 1, Nothing), (Game.Player 2, Just $ Move 5 4), (Game.Player 3, Just $ Move 6 8)]
handleTurns :: Game.Game -> [Handle] -> IO Game.FinishedGame
handleTurns game handles = do
  replyAll Turn handles
  playerCommands <-
    mapM
      (\(idx, handle) -> do
         maybeCommand <- readCommand handle
         return (Game.Player idx, maybeCommand)) $
    zip [1 ..] handles
  let turn = getTurn game playerCommands
  case Game.play game turn of
    Left finishedGame -> return finishedGame
    Right game' -> handleTurns game' handles

runGame :: [(Socket, SockAddr)] -> IO ()
runGame conns = do
  handles <- mapM getConnectionHandle conns
  mapM_ sendHello handles
  let game = Game.emptyGame
  finishedGame <- handleTurns game handles
  -- putStrLn $ "Finished game " ++ show finishedGame
  print finishedGame
  mapM_ hClose handles
