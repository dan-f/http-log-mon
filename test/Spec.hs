import Test.Hspec
import Test.QuickCheck
import Control.Exception
  ( evaluate
  )

import qualified Data.Time as T

import qualified Alert as A
import RequestLog
  ( Request(..)
  , RequestLog
  , emptyRequestLog
  )


buildReqLog :: T.UTCTime -> Int -> RequestLog
buildReqLog currentTime hits =
  replicate hits req
  where
    oneMinuteAgo = 0 - 60
    reqTime = oneMinuteAgo `T.addUTCTime` currentTime
    req = Request { reqIp = "127.0.0.1"
                  , reqIdent = Nothing
                  , reqUserId = Nothing
                  , reqTime = reqTime
                  , reqMethod = "GET"
                  , reqPath = []
                  , reqProtocol = "HTTP/1.1"
                  , reqStatus = 200
                  , reqRespSize = 1
                  }


main :: IO ()
main = hspec $ do
  describe "updateAlertHistory" $ do
    describe "when traffic for the last two minutes warrants an alert" $ do
      describe "and there are no alerts" $ do
        it "adds a new alert" $ do
          let day = T.fromGregorianValid 2017 1 1
          case day of
            Nothing ->
              expectationFailure "Could not create date necessary for test"
            Just d ->
              let
                hits = 100
                reqLog = buildReqLog curTime hits
                curTime = T.UTCTime d 60
                justOneAlert = A.push (A.newAlert curTime hits) A.emptyAlertHistory
              in
                (A.updateAlertHistory curTime reqLog A.emptyAlertHistory) `shouldBe` justOneAlert

      describe "and there is an inactive alert" $ do
        it "adds a new alert" $ do
          let day = T.fromGregorianValid 2017 1 1
          case day of
            Nothing ->
              expectationFailure "Could not create date necessary for test"
            Just d ->
              let
                hits = 100
                reqLog = buildReqLog curTime hits
                curTime = T.UTCTime d 60
                oldAlertHistory = A.recover curTime $ A.push (A.newAlert curTime 999) A.emptyAlertHistory
                newAlertHistory = A.push (A.newAlert curTime hits) oldAlertHistory
              in
                (A.updateAlertHistory curTime reqLog oldAlertHistory) `shouldBe` newAlertHistory

      describe "and there is an active alert" $ do
        it "updates the current alert" $ do
          let day = T.fromGregorianValid 2017 1 1
          case day of
            Nothing ->
              expectationFailure "Could not create date necessary for test"
            Just d ->
              let
                originalHits = 100
                reqLog = buildReqLog curTime originalHits
                newReqLog = reqLog ++ reqLog -- add some new hits
                curTime = T.UTCTime d 60
                originalAlertHistory = A.push (A.newAlert curTime originalHits) A.emptyAlertHistory
                expectedAlertHistory = A.push (A.newAlert curTime (originalHits * 2)) A.emptyAlertHistory
              in
                (A.updateAlertHistory curTime newReqLog originalAlertHistory) `shouldBe` expectedAlertHistory

    describe "when traffic for the last two minutes does not warrant an alert" $ do
      describe "and there are no alerts" $ do
        it "does not change the alert history" $ do
          let day = T.fromGregorianValid 2017 1 1
          case day of
            Nothing ->
              expectationFailure "Could not create date necessary for test"
            Just d ->
              let
                hits = 10
                reqLog = buildReqLog curTime hits
                curTime = T.UTCTime d 60
                history = A.emptyAlertHistory
              in
                (A.updateAlertHistory curTime reqLog history) `shouldBe` history

      describe "and there is a closed alert" $ do
        it "does not change the alert history" $ do
          let day = T.fromGregorianValid 2017 1 1
          case day of
            Nothing ->
              expectationFailure "Could not create date necessary for test"
            Just d ->
              let
                hits = 10
                reqLog = buildReqLog curTime hits
                curTime = T.UTCTime d 60
                history = A.recover curTime $ A.push (A.newAlert curTime hits) A.emptyAlertHistory
              in
                (A.updateAlertHistory curTime reqLog history) `shouldBe` history

      describe "and there is an active alert" $ do
        it "closes the active alert" $ do
          let day = T.fromGregorianValid 2017 1 1
          case day of
            Nothing ->
              expectationFailure "Could not create date necessary for test"
            Just d ->
              let
                hits = 50
                reqLog = buildReqLog curTime hits
                curTime = T.UTCTime d 60
                oldHistory = A.push (A.newAlert curTime hits) A.emptyAlertHistory
                newHistory = A.recover curTime oldHistory
              in
                (A.updateAlertHistory curTime reqLog oldHistory) `shouldBe` newHistory
