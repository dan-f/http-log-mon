module Lib
  ( parseLogFile
  ) where

import Data.Char
import Data.Maybe
import Data.Time
import Text.ParserCombinators.Parsec

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

type Log = [Request]

parseLogFile :: String -> Either ParseError [Request]
parseLogFile =
  parse logFile "(unknown)"

logFile :: GenParser Char st [Request]
logFile = do
  possibleRequests <- request `sepEndBy` eol
  optional eof
  return $ catMaybes possibleRequests

request :: GenParser Char st (Maybe Request)
request = do
  ip' <- ip
  spaces
  ident' <- possibleAlphaNum
  spaces
  userId' <- possibleAlphaNum
  spaces
  possibleTime <- time
  spaces
  char '"'
  method' <- method
  spaces
  path' <- path
  spaces
  protocol' <- protocol
  char '"'
  spaces
  status' <- positiveInt
  spaces
  respSize' <- positiveInt
  case possibleTime of
    Nothing ->
      return Nothing
    Just time' ->
      return (Just $ Request ip' ident' userId' time' method' path' protocol' status' respSize')

ip :: GenParser Char st String
ip =
  many1 (oneOf "0123456789.") <?> "ip address"

possibleAlphaNum :: GenParser Char st (Maybe String)
possibleAlphaNum = do
  parsed <- (many1 alphaNum) <|> (string "-") <?> "word or \"-\""
  if parsed == "-"
    then return Nothing
    else return $ Just parsed

time :: GenParser Char st (Maybe UTCTime)
time = do
  char '['
  dateTimeStr <- manyTill anyChar (char ']') <?> "time string"
  return $ parseTimeM
    False
    defaultTimeLocale
    "%d/%b/%Y:%H:%M:%S %z"
    dateTimeStr

method :: GenParser Char st String
method =
  many1 letter <?> "method"

path :: GenParser Char st [String]
path = do
  char '/'
  pathSegment `sepEndBy` (char '/')

pathSegment :: GenParser Char st String
pathSegment =
  many1 (noneOf ['/', ' '])

protocol :: GenParser Char st String
protocol =
  try (string "HTTP/1.0") <|>
  try (string "HTTP/2.0") <|>
  try (string "HTTP/1") <|>
  try (string "HTTP/2")
  <?> "protocol"

positiveInt :: GenParser Char st Int
positiveInt = do
  digits <- many1 digit <?> "digits"
  return $ foldl (\acc i -> acc * 10 + digitToInt i) 0 digits

eol :: GenParser Char st String
eol =
  try (string "\n\r") <|>
  try (string "\r\n") <|>
  (string "\n") <|>
  (string "\r")
  <?> "end of line"
