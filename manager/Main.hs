{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import System.Directory
-- import Database.PostgreSQL.Simple

-- hello :: IO Int
-- hello = do
--   conn <- connectPostgreSQL "postgres://aiplay:aiplay@localhost/aiplay"
--   [Only i] <- query_ conn "select 2 + 5"
--   return i

main :: IO ()
main = do
  putStrLn "hello world"
