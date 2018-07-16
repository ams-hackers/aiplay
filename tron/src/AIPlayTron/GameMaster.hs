{-# LANGUAGE OverloadedStrings #-}

module AIPlayTron.GameMaster where

import Control.Exception (finally)
import Control.Monad
import Data.Char (isSpace)
import Data.List
import Network.Socket
import System.IO

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
  finally (mainLoop sock) (close sock)

mainLoop :: Socket -> IO ()
mainLoop sock = do
  conns <- acceptPlayers sock numberOfPlayers
  runGame conns

readLine :: Handle -> IO String
readLine handle = dropWhileEnd isSpace <$> hGetLine handle

readCommand :: Handle -> IO (Maybe Command)
readCommand handle = parseCommand <$> readLine handle

reply :: Result -> Handle -> IO ()
reply result handle = hPutStrLn handle $ formatResult result

sendHello :: Handle -> IO ()
sendHello handle = do
  reply (Welcome 1) handle
  reply (Size 100 100) handle
  reply (Wall (1, 1)) handle
  reply (Wall (2, 3)) handle
  reply (Player 1 (10, 10)) handle
  reply (You (1, 2)) handle

handleTurns :: [Handle] -> IO ()
handleTurns handles = do
  broadcast "TURN" handles
  responses <-
    mapM
      (\handle -> do
         line <- readLine handle
         return (handle, line))
      handles
  mapM_ (\(handle, line) -> broadcast line (delete handle handles)) responses

runGame :: [(Socket, SockAddr)] -> IO ()
runGame conns = do
  handles <- mapM getConnectionHandle conns
  mapM_ sendHello handles
  handleTurns handles
  mapM_ hClose handles
  return ()
