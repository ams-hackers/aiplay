module AIPlayTron.Socket
  ( module AIPlayTron.Socket
  , PortNumber
  ) where

import Control.Monad
import Network.Socket
import System.IO

listenPlayers :: Int -> PortNumber -> IO Socket
listenPlayers numberOfPlayers port = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet port iNADDR_ANY)
  listen sock numberOfPlayers -- set a max of 2 queued connections
  return sock

acceptPlayers :: Socket -> Int -> IO [(Socket, SockAddr)]
acceptPlayers sock n = replicateM n (accept sock)

getConnectionHandle :: (Socket, SockAddr) -> IO Handle
getConnectionHandle (sock, _) = do
  handle <- socketToHandle sock ReadWriteMode
  hSetBuffering handle NoBuffering
  return handle
