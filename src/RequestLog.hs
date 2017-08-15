module RequestLog
  ( Request(..)
  , RequestLog
  , emptyRequestLog
  , totalTraffic
  ) where

import Data.Time


data Request = Request { reqIp :: String
                       , reqIdent :: Maybe String
                       , reqUserId :: Maybe String
                       , reqTime :: UTCTime
                       , reqMethod :: String
                       , reqPath :: [String]
                       , reqProtocol :: String
                       , reqStatus :: Int
                       , reqRespSize :: Int
                       } deriving (Show)

type RequestLog = [Request]


emptyRequestLog :: RequestLog
emptyRequestLog = []


-- Computes the total number of requests in the log in a given time range
totalTraffic :: UTCTime -> UTCTime -> RequestLog -> Int
totalTraffic startT endT =
  foldl
    (\hits req ->
      let
        thisT = reqTime req
      in
        if (startT <= thisT && thisT <= endT)
          then hits + 1
          else hits
    )
    0
