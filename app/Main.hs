module Main where

import System.IO

import Lib

main :: IO ()
main = do
  contents <- readFile "logfile.txt"
  print $ parseLogFile contents
