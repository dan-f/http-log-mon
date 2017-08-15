module Main where

import Control.Concurrent
  ( forkIO
  , threadDelay
  )
import Control.Monad
  ( forever
  , void
  )
import System.Environment
import System.IO

import qualified Brick.BChan as BC
import Data.Time
  ( getCurrentTime
  )

import App
import RequestLogParse

-- The application will consist of three lightweight threads:
--   * The main application thread which renders the logs
--   * A thread to count time
--   * The log reading/parsing thread
-- There is a bounded FIFO channel for communication which is passed off to
-- the main thread.
main :: IO ()
main = void $ do
  args <- getArgs
  let filename = if null args then "logfile.txt" else head args
      oneSecond = 1000000 -- microseconds
  channel <- BC.newBChan 10
  -- countdown thread
  forkIO $ forever $ do
    threadDelay oneSecond
    currentTime <- getCurrentTime
    BC.writeBChan channel (Tick currentTime)
  -- file parsing thread
  forkIO $ forever $ do
    parsedLogM <- readLogFile filename
    case parsedLogM of
      Left err -> do
        BC.writeBChan channel (LogError $ "Could not parse the log file:\n\n\t" ++ (show err))
      Right parsedLog ->
        BC.writeBChan channel (LogRead parsedLog)
    threadDelay $ oneSecond * 10
  App.run channel
