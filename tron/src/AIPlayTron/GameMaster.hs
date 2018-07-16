{-# LANGUAGE OverloadedStrings #-}

module AIPlayTron.GameMaster where

import Control.Exception (finally)

import Network.Socket

--import Network.Socket.ByteString
import System.IO

port :: PortNumber
port = 4242

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet port iNADDR_ANY)
  listen sock 2 -- set a max of 2 queued connections
  putStrLn $ "Listening on " ++ show port
  finally (mainLoop sock) (close sock)

mainLoop :: Socket -> IO ()
mainLoop sock = do
  conn <- accept sock
  runConn conn
  mainLoop sock

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
  handle <- socketToHandle sock ReadWriteMode
  hSetBuffering handle NoBuffering
  hPutStrLn handle "HELLO\n"
  hClose handle
