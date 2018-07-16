{-# LANGUAGE OverloadedStrings #-}

module AIPlayTron.GameMaster where

import Control.Exception (finally)
import Control.Monad
import Data.Char (isSpace)
import Data.List

import Network.Socket

--import Network.Socket.ByteString
import System.IO

port :: PortNumber
port = 4242

numberOfPlayers :: Int
numberOfPlayers = 2

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet port iNADDR_ANY)
  listen sock numberOfPlayers -- set a max of 2 queued connections
  putStrLn $ "Listening on " ++ show port
  finally (mainLoop sock) (close sock)

acceptPlayers :: Socket -> Int -> IO [(Socket, SockAddr)]
acceptPlayers sock n = replicateM n (accept sock)

mainLoop :: Socket -> IO ()
mainLoop sock = do
  conns <- acceptPlayers sock numberOfPlayers
  runGame conns

readLine :: Handle -> IO String
readLine handle = dropWhileEnd isSpace <$> hGetLine handle

sendHello :: Handle -> IO ()
sendHello handle = do
  hPutStrLn handle "TRON 1"
  hPutStrLn handle "SIZE 100 100"
  hPutStrLn handle "WALL 1 1"
  hPutStrLn handle "WALL 2 3"
  hPutStrLn handle "PLAYER 10 10"
  hPutStrLn handle "YOU 1 2"

broadcast :: String -> [Handle] -> IO ()
broadcast msg = mapM_ (\h -> hPutStrLn h msg)

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

getConnectionHandle :: (Socket, SockAddr) -> IO Handle
getConnectionHandle (sock, _) = do
  handle <- socketToHandle sock ReadWriteMode
  hSetBuffering handle NoBuffering
  return handle

runGame :: [(Socket, SockAddr)] -> IO ()
runGame conns = do
  handles <- mapM getConnectionHandle conns
  mapM_ sendHello handles
  handleTurns handles
  mapM_ hClose handles
  return ()
