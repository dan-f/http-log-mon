module Alert
  ( Alert(..)
  , AlertHistory
  , emptyAlertHistory
  , newAlert
  , push
  , recover
  , lastAlert
  , hasActiveAlert
  , updateAlertHistory
  ) where

import Data.Maybe
  ( isJust
  )

import Data.Time
  ( UTCTime
  , addUTCTime
  )

import RequestLog

data Alert = Alert { triggered :: UTCTime
                   , recovered :: Maybe UTCTime
                   , hits :: Int
                   } deriving (Show, Eq)


type AlertHistory = [Alert]


-- Constant for the threshold of requests within the last two minutes that
-- should trigger an alert.
hitThreshold :: Int
hitThreshold = 100


emptyAlertHistory :: AlertHistory
emptyAlertHistory = []


newAlert :: UTCTime -> Int -> Alert
newAlert triggered hits =
  Alert { triggered = triggered
        , hits = hits
        , recovered = Nothing
        }


push :: Alert -> AlertHistory -> AlertHistory
push alert history =
  alert : history


lastAlert :: AlertHistory -> Maybe Alert
lastAlert history =
  case history of
    [] ->
      Nothing
    (a:as) ->
      Just a


hasActiveAlert :: AlertHistory -> Bool
hasActiveAlert history =
  case lastAlert history of
    Nothing ->
      False
    Just alert ->
      isActive alert


isActive :: Alert -> Bool
isActive =
  not . isJust . recovered


recover :: UTCTime -> AlertHistory -> AlertHistory
recover currentTime history =
  case history of
    [] ->
      []
    a:as ->
      (a { recovered = Just currentTime }) : as


updateAlertHistory :: UTCTime -> RequestLog -> AlertHistory -> AlertHistory
updateAlertHistory currentTime reqLog history
  | alertShouldBeActive && (not alertIsActive) = push (newAlert currentTime hits) history
  | alertShouldBeActive && alertIsActive       = push (newAlert currentTime hits) (tail history)
  | (not alertShouldBeActive) && alertIsActive = recover currentTime history
  | otherwise                                  = history
  where
    lastTwoMinutes = 0 - 120
    startTime = lastTwoMinutes `addUTCTime` currentTime
    hits = totalTraffic startTime currentTime reqLog
    alertShouldBeActive = hits >= hitThreshold
    alertIsActive = hasActiveAlert history
