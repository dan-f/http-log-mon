module RequestLog
  ( Request(..)
  , RequestLog
  , emptyRequestLog
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
