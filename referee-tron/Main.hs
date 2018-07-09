module Main where

import System.Environment (getArgs)
import System.Exit (die)

mainWithArgs :: [String] -> IO ()
mainWithArgs [url1, url2] = do
 putStrLn $ "Player 1: " ++ url1
 putStrLn $ "Player 2: " ++ url2

mainWithArgs _ =
   die "Usage: aiplay-referee-tron <url1> <url2>"

main :: IO ()
main = getArgs >>= mainWithArgs

