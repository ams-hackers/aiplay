{-# LANGUAGE OverloadedStrings #-}

module AIPlayTron.GameMaster where

import Control.Exception (finally)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isSpace)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as IO
import Network.Socket
import System.IO

import qualified AIPlayTron.Game as Game
import AIPlayTron.Protocol
import AIPlayTron.Socket

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

handleTurns :: Game.Game -> [Handle] -> IO Game.FinishedGame
handleTurns game handles = do
  replyAll Turn handles
  playerCommands <-
    mapM
      (\(idx, handle) -> do
         maybeCommand <- readCommand handle
         return (Game.Player idx, maybeCommand)) $
    zip [1 ..] handles
  let turn =
        Map.fromList
          [ (player, Game.Coord (x, y))
          | (player, Just (Move x y)) <- playerCommands
          ]
  case Game.play game turn of
    Left finishedGame -> return finishedGame
    Right game' -> handleTurns game' handles

runGame :: [(Socket, SockAddr)] -> IO ()
runGame conns = do
  handles <- mapM getConnectionHandle conns
  mapM_ sendHello handles
  let game = Game.emptyGame
  finishedGame <- handleTurns game handles
  LBS.writeFile "game-history.json" $ encode finishedGame
  mapM_ hClose handles
