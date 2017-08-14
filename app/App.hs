module App where

import qualified Data.List as List
import qualified Data.Map as Map
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
  , textWidth
  , vBox
  , vLimit
  )
import qualified Brick.Widgets.List as L
import qualified Graphics.Vty as V

import RequestLog


-- Data


-- Custom event type for app-specific events
data AppEvent = LogRead RequestLog
              | LogError String
              | SecondTick


-- Name type for identifying widgets
data Name = TopSections
          deriving (Ord, Eq, Show)


-- Holds the application state
data AppState = AppState { requestLog :: RequestLog
                         , lastError :: String
                         , secondsUntilUpdate :: Int
                         }


-- Initialization


-- Runs the app
run :: BC.BChan AppEvent -> IO AppState
run channel =
  M.customMain
    (V.mkVty V.defaultConfig)
    (Just channel)
    app
    initialState


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
initialState :: AppState
initialState =
  AppState { requestLog = emptyRequestLog
           , lastError = ""
           , secondsUntilUpdate = 10
           }


-- Events


-- Handles all types of events (App events, VTY events) by updating app state
-- and possibly firing new events
handleEvent :: AppState -> T.BrickEvent Name AppEvent -> T.EventM Name (T.Next AppState)
handleEvent s (T.AppEvent (LogRead newLog)) =
  M.continue s { requestLog = newLog }
handleEvent s (T.AppEvent SecondTick) =
  let
    t = secondsUntilUpdate s
  in
    M.continue s { secondsUntilUpdate = if t > 1 then t - 1 else 10 }
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
        -- , C.hCenter (alerts $ requestLog s)
        ]
  ]


timeUntilUpdate :: Int -> T.Widget Name
timeUntilUpdate seconds =
  str $ "[ Reading new log data in " ++ (show seconds) ++ " second(s) ]"


statistics :: RequestLog -> T.Widget Name
statistics l =
  B.borderWithLabel
    (str "Statistics") $
    vBox
      [ str $ "Total hits: " ++ (show $ length l)
      , str $ "Unique IPs: "
      , str $ "Mean. response size: "
      , str $ "Median. response size: "
      , str $ "Largest pages: "
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
    hLimit 50 $
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
        sectionName = if null path then "/" else ("/" ++ head path)
      in
        if (Map.member sectionName m)
          then Map.update
                 (\hits -> Just (hits + 1))
                 sectionName
                 m
          else Map.insert sectionName 1 m
    )
    Map.empty
