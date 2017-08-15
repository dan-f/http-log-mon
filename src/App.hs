module App where

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Time
  ( UTCTime
  , addUTCTime
  , getCurrentTime
  , secondsToDiffTime
  )
import qualified Data.Set as S
import qualified Data.Vector as Vec

import qualified Graphics.Vty as V

import qualified Brick.AttrMap as A
import qualified Brick.BChan as BC
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , fill
  , hLimit
  , padLeft
  , padRight
  , str
  , strWrap
  , textWidth
  , vBox
  , vLimit
  )
import qualified Brick.Widgets.List as L
import qualified Graphics.Vty as V

import Alert
import RequestLog


-- Data


-- Custom event type for app-specific events
data AppEvent = LogRead RequestLog
              | LogError String
              | Tick UTCTime


-- Name type for identifying widgets
data Name = TopSections
          | Alerts
          deriving (Ord, Eq, Show)


-- Holds the application state
data AppState = AppState { requestLog :: RequestLog
                         , secondsUntilUpdate :: Int
                         , currentTime :: UTCTime
                         , alertHistory :: AlertHistory
                         }


-- Initialization


-- Runs the app
run :: BC.BChan AppEvent -> IO AppState
run channel = do
  currentTime <- getCurrentTime
  M.customMain
    (V.mkVty V.defaultConfig)
    (Just channel)
    app
    (initialState currentTime)


-- App definition for Brick
app :: M.App AppState AppEvent Name
app =
  M.App { M.appDraw = root
        , M.appChooseCursor = M.neverShowCursor
        , M.appHandleEvent = handleEvent
        , M.appStartEvent = return
        , M.appAttrMap = const theMap
        }


-- First value for the application state
initialState :: UTCTime -> AppState
initialState currentTime =
  AppState { requestLog = emptyRequestLog
           , secondsUntilUpdate = 10
           , currentTime = currentTime
           , alertHistory = emptyAlertHistory
           }


-- Events


-- Handles all types of events (App events, VTY events) by updating app state
-- and possibly firing new events
handleEvent :: AppState -> T.BrickEvent Name AppEvent -> T.EventM Name (T.Next AppState)
handleEvent s (T.AppEvent (LogRead newLog)) =
  M.continue s { requestLog = newLog }
handleEvent s (T.AppEvent (Tick newTime)) =
  M.continue s { currentTime = newTime
               , secondsUntilUpdate = if t > 1 then t - 1 else 10
               , alertHistory = updateAlertHistory newTime (requestLog s) (alertHistory s)
               }
  where t = secondsUntilUpdate s
handleEvent s (T.VtyEvent (V.EvKey (V.KChar 'q') [])) =
  M.halt s
handleEvent s (T.VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) =
  M.halt s
handleEvent s (T.VtyEvent (V.EvKey V.KEsc [])) =
  M.halt s
handleEvent s (T.VtyEvent (V.EvKey V.KEnter [])) =
  M.halt s
handleEvent s _ =
  M.continue s


-- Widgets


-- The root-level widget
root :: AppState -> [T.Widget Name]
root s =
  [ B.borderWithLabel
      (str "HTTP Log Monitor") $
      vBox
        [ C.hCenter (timeUntilUpdate $ secondsUntilUpdate s)
        , C.hCenter $ (topSections $ requestLog s) <+> (statistics $ requestLog s)
        , C.hCenter $ alerts (alertHistory s)
        ]
  ]


timeUntilUpdate :: Int -> T.Widget Name
timeUntilUpdate seconds =
  strWrap $ "[ Reading new log data in " ++ (show seconds) ++ " second(s) ]"


statistics :: RequestLog -> T.Widget Name
statistics l =
  B.borderWithLabel
    (str "Statistics") $
    vBox
      [ str $ "Total hits: " ++ (show $ length l)
      , str $ "Unique IPs: " ++ (show $ uniqueIpAddrs l)
      , str $ "Mean response size: " ++ (show $ meanRespSize l)
      , str $ "Median response size: " ++ (show $ medianRespSize l)
      ]


topSections :: RequestLog -> T.Widget Name
topSections l =
  let
    sectionMap = buildSectionMap l
    sortedSections =
      List.sortOn (\(section, hits) -> 0 - hits) (Map.assocs sectionMap)
    brickList =
      L.list
        TopSections
        (Vec.fromList sortedSections)
        1
  in
    B.borderWithLabel (str "Top Sections") $
      vBox
        [ (vLimit 1 $ str "Section" <+> fill ' ' <+> str "Hits")
        , L.renderList
            section
            True
            brickList
        ]


section :: Bool -> (String, Int) -> T.Widget Name
section hasFocus (sectionName, hits) =
  let
    pathWidget = str sectionName
    hitCountWidget = str (show hits)
  in
    vLimit 1 $
    pathWidget <+> (fill '.') <+> hitCountWidget


alerts :: [Alert] -> T.Widget Name
alerts alertHistory =
  let
    brickList =
      L.list
        Alerts
        (Vec.fromList alertHistory)
        1
  in
    B.borderWithLabel (str "Alerts") $
      L.renderList
        alert
        True
        brickList

alert :: Bool -> Alert -> T.Widget Name
alert hasFocus a =
  let
    alertText = "* High traffic generated an alert - hits = " ++ (show $ hits a) ++ ", triggered at " ++ (show $ triggered a)
    timeRecovered = recovered a
  in
    case timeRecovered of
      Nothing ->
        strWrap alertText
      Just t ->
        strWrap $ alertText ++ "[Recovered at " ++ (show t) ++ "]"

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    []


-- Useful Computations


buildSectionMap :: RequestLog -> Map.Map String Int
buildSectionMap =
  foldl
    (\m req ->
      let
        path = reqPath req
        sectionName = if null path then "/" else ("/" ++ (clean $ head path))
      in
        if (Map.member sectionName m)
          then Map.update
                 (\hits -> Just (hits + 1))
                 sectionName
                 m
          else Map.insert sectionName 1 m
    )
    Map.empty
  where
    clean = takeWhile (/= '?')


uniqueIpAddrs :: RequestLog -> Int
uniqueIpAddrs reqLog =
  length $
    foldl
      (\ipSet req ->
        S.insert (reqIp req) ipSet
      )
      S.empty
      reqLog


meanRespSize :: RequestLog -> Int
meanRespSize reqLog =
  if (null respSizes)
    then 0
    else (sum respSizes) `quot` (length respSizes)
  where
    respSizes = map reqRespSize reqLog


medianRespSize :: RequestLog -> Int
medianRespSize reqLog =
  if (null respSizes)
    then 0
    else respSizes !! (length respSizes `quot` 2)
  where
    respSizes = map reqRespSize reqLog
