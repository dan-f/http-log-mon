module Main where

import System.IO

import LogParse

main :: IO ()
main = do
  contents <- readFile "logfile.txt"
  print $ parseLogFile contents
