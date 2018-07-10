{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Directory
import Database.PostgreSQL.Simple


hello :: IO Int
hello = do
  conn <- connectPostgreSQL "postgres://aiplay:aiplay@localhost/aiplay"
  [Only i] <- query_ conn "select 2 + 2"
  return i

main :: IO ()
main = do
  n <- hello
  putStrLn (show n)
