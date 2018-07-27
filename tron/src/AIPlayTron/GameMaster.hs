{-# LANGUAGE OverloadedStrings #-}

module AIPlayTron.GameMaster where

import AIPlayTron.Protocol
import AIPlayTron.Socket
import Control.Exception (finally)
import Data.Char (isSpace)
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

handleTurns :: [Handle] -> IO ()
handleTurns handles = do
  replyAll Turn handles
  _responses <-
    mapM
      (\handle -> do
         line <- readLine handle
         return (handle, line))
      handles
  return ()

runGame :: [(Socket, SockAddr)] -> IO ()
runGame conns = do
  handles <- mapM getConnectionHandle conns
  mapM_ sendHello handles
  handleTurns handles
  mapM_ hClose handles
  return ()
