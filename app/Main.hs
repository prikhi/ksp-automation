{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Brick
import Brick.BChan (newBChan, writeBChan)
import Control.Exception (SomeException, catch, bracket)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO, killThread)
import Control.Concurrent.Async (Async, async, cancel)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar, modifyMVar_)
import Data.Default (Default(def))
import Data.Maybe (listToMaybe)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import GHC.Conc (TVar, newTVarIO)
import KRPCHS (withRPCClient, withStreamClient, runRPCProg)

import KSP.Automation

import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import qualified Data.Text as T
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V


main :: IO ()
main =
        bracket startTick (killThread . snd) (runUI . fst)
    where
        -- Create a `BChan` in a new thread that produces a `Tick` every 250ms
        startTick = do
            chan <- newBChan 10
            tickThreadId <- forkIO . forever $ do
                writeBChan chan Tick
                threadDelay 250000
            return (chan, tickThreadId)
        -- Run the UI loop, cleanup the async Mission thread on exit
        -- TODO: Use MVar to cleanup async thread on main thread exception?
        runUI tickChannel =
            initialState
                >>= customMain (V.mkVty V.defaultConfig) (Just tickChannel) appConfig
                >>= maybe (return ()) cancel . asMissionThread


-- DATA

-- Widget Names
data Name
    = MissionList
    | LogList
    deriving (Eq, Ord, Show)

-- Application Events
data Tick
    = Tick


-- The app's Brick configuration.
appConfig :: App AppState Tick Name
appConfig =
    App
        { appDraw = renderApp
        , appChooseCursor = const listToMaybe
        , appHandleEvent = handleEvents
        , appStartEvent = return
        , appAttrMap = const styleMap
        }

-- The state used within the Brick app.
data AppState =
    AppState
        { asFocusedSection :: AppSection
        , asMissionLog :: L.List Name LogEntry
        , asMissionInProgress :: Bool
        , asGlobalOptions :: TVar GlobalOptions
        , asMissionList :: L.List Name Mission
        , asMissionThread :: Maybe (Async ())
        , asLogEntriesMVar :: MVar [LogEntry]
        }

initialState :: IO AppState
initialState = do
    loggerMVar <- newMVar []
    logEntry <- makeLogEntry "KSP Automation Initialized - Awaiting Mission Selection."
    options <- newTVarIO def
    return AppState
        { asFocusedSection = MissionSelect
        , asMissionLog = L.list LogList (Vec.fromList [logEntry]) 1
        , asMissionInProgress = False
        , asGlobalOptions = options
        , asMissionList = L.list MissionList (Vec.fromList [minBound .. maxBound]) 1
        , asMissionThread = Nothing
        , asLogEntriesMVar = loggerMVar
        }

-- The different sections of the UI the user can cycle through
data AppSection
    = MissionSelect
    | Parameters
    | FlightLog
    | Options
    deriving (Eq, Enum, Bounded)

instance Show AppSection where
    show = \case
        MissionSelect ->
            "Mission Selection"
        Parameters ->
            "Parameters"
        FlightLog ->
            "Flight Log"
        Options ->
            "Options"

-- | The mission programs available.
data Mission
    = ExecuteManeuver
    | SubOrbital
    | LowKerbinOrbit
    | LiftOff
    | Circularize
    deriving (Enum, Bounded)

instance Show Mission where
    show = \case
        LowKerbinOrbit ->
            "Launch to Low Kerbin Orbit"
        LiftOff ->
            "Launchpad Lift-Off"
        Circularize ->
            "Circularize Orbit at Apoapsis"
        ExecuteManeuver ->
            "Execute Planned Maneuver"
        SubOrbital ->
            "Sub-Orbital Launch"


data LogEntry
    = LogEntry
        { leDate :: T.Text
        , leMessage :: T.Text
        }

makeLogEntry :: T.Text -> IO LogEntry
makeLogEntry msg = do
    currentTime <- getCurrentTime
    return LogEntry
        { leDate = T.pack $ formatTime defaultTimeLocale "%T" currentTime
        , leMessage = msg
        }



-- STYLES

defaultStyle :: AttrName
defaultStyle =
    attrName "default"

logBrackets :: AttrName
logBrackets =
    attrName "logBrackets"

logDate :: AttrName
logDate =
    attrName "logDate"

logMessage :: AttrName
logMessage =
    attrName "logMessage"

inputLabelAttr :: AttrName
inputLabelAttr =
    attrName "inputLabel"

focusedBorder :: AttrName
focusedBorder =
    attrName "focusedBorder"

focusedActiveListItem :: AttrName
focusedActiveListItem =
    attrName "focusedActiveListItem"

unfocusedActiveListItem :: AttrName
unfocusedActiveListItem =
    attrName "unfocusedActiveListItem"

focusedListItem :: AttrName
focusedListItem =
    attrName "focusedListItem"

unfocusedListItem :: AttrName
unfocusedListItem =
    attrName "unfocusedListItem"

focusedBorderColor :: AttrName
focusedBorderColor =
    attrName "focusedBorderColor"

unfocusedBorderColor :: AttrName
unfocusedBorderColor =
    attrName "unfocusedBorderColor"

styleMap :: AttrMap
styleMap =
    attrMap (fg V.magenta)
        [ ( defaultStyle, fg V.magenta )
        -- Form Inputs
        , ( inputLabelAttr, V.withStyle (fg V.magenta) V.bold )
        -- Section Borders
        , ( focusedBorderColor, fg V.magenta )
        , ( unfocusedBorderColor, fg V.white )
        -- Mission List
        , ( focusedActiveListItem, V.withStyle (V.black `on` V.magenta) V.bold )
        , ( unfocusedActiveListItem, V.withStyle (V.white `on` V.magenta) V.bold )
        , ( focusedListItem, fg V.magenta )
        , ( unfocusedListItem, fg V.white )
        -- Flight Log
        , ( logBrackets, fg V.white )
        , ( logDate, fg V.magenta )
        , ( logMessage, fg V.white )
        ]


-- RENDER

renderApp :: AppState -> [Widget Name]
renderApp s =
    let
        mainWindow =
                renderLeft s
            <+> renderRight s
            <=> str "STATUS BAR"
    in
        [ mainWindow
        ]


renderLeft :: AppState -> Widget Name
renderLeft s =
    let
        longestMissionText =
            maximum
                $ map (textWidth . show) [minBound .. maxBound :: Mission]
    in
        vBox
            [ renderSection MissionSelect s
                . hLimit (longestMissionText + 4)
                $ renderMissionList s
            , renderSection Options s
                $ renderOptions s
            ]


renderMissionList :: AppState -> Widget Name
renderMissionList s =
    let
        isFocusedSection =
            asFocusedSection s == MissionSelect
    in
        L.renderList renderMission isFocusedSection (asMissionList s)
    where
        renderMission isSelected mission =
            let
                attribute
                    | isSelected && asFocusedSection s == MissionSelect =
                        withAttr focusedActiveListItem
                    | isSelected =
                        withAttr unfocusedActiveListItem
                    | asFocusedSection s == MissionSelect  =
                        withAttr focusedListItem
                    | otherwise =
                        withAttr unfocusedListItem
            in
                 attribute
                    . padLeft (Pad 1)
                    . padRight Max
                    . str
                    $ show mission


-- TODO: Implement Form Fields
-- https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst#input-forms
renderOptions :: AppState -> Widget Name
renderOptions _ =
    padTopBottom 1 . padLeftRight 2 $ vBox
        [ hBox
            [ withAttr inputLabelAttr
                . padRight (Pad 3)
                $ str "Enable Auto-Staging:"
            , str "[ X ]"
            ]
        ]


renderRight :: AppState -> Widget Name
renderRight s =
        renderSection Parameters s (renderMissionParameters s )
    <=> renderSection FlightLog s (renderMissionLog (asMissionLog s))


-- TODO: Implement Brick.Form Fields
renderMissionParameters :: AppState -> Widget Name
renderMissionParameters _ =
    padTopBottom 1 $ C.hCenter $ hBox
        [ withAttr inputLabelAttr $ padRight (Pad 2) $ vBox
            [ str "Target Apoapsis(meters):"
            , str "Match Target Inclination:"
            ]
        , vBox
            [ str "[ 75,000 ]"
            , str "     [ X ]"
            ]
        ]


renderMissionLog :: L.List Name LogEntry -> Widget Name
renderMissionLog =
        L.renderList renderEntry False
    where
        renderEntry _ entry =
            padRight Max $
            withAttr logBrackets (str "[")
                <+> withAttr logDate (txt $ leDate entry)
                <+> withAttr logBrackets (str "] ")
                <+> withAttr logMessage (txtWrap $ leMessage entry)


renderSection :: AppSection -> AppState -> Widget n -> Widget n
renderSection section state =
    let
        isFocused =
            asFocusedSection state == section
        borderLabel =
            B.borderWithLabel (str $ " " ++ show section ++ " ")
                . withDefAttr defaultStyle
    in
        if isFocused then
            withDefAttr focusedBorderColor
                . withBorderStyle B.unicodeBold
                . borderLabel
        else
            withDefAttr unfocusedBorderColor
                . borderLabel


-- UPDATE

-- TODO: This should just dispatch to other functions
handleEvents :: AppState -> BrickEvent Name Tick -> EventM Name (Next AppState)
handleEvents s = \case
    -- Add Log Entries from Shared MVar to AppState on Every Tick
    AppEvent Tick -> do
        -- TODO: Poll async mission thread & update state
        newLogEntries <- liftIO . modifyMVar (asLogEntriesMVar s)
            $ \entries -> return ([], reverse entries)
        continue s
            { asMissionLog =
                foldl (\ml le -> L.listMoveDown $ L.listInsert maxBound le ml)
                    (asMissionLog s)
                    newLogEntries
            }

    VtyEvent ev -> do
        let focusedSection =
                asFocusedSection s
        updatedState <-
            case focusedSection of
                MissionSelect ->
                    (\l -> s { asMissionList = l })
                        <$> L.handleListEventVi L.handleListEvent ev (asMissionList s)
                FlightLog ->
                    (\l -> s { asMissionLog = l })
                        <$> L.handleListEventVi L.handleListEvent ev (asMissionLog s)
                _ ->
                    return s

        -- TODO: Split each section's keybindings into separate function
        case ev of
            -- Launch Mission
            V.EvKey V.KEnter [] ->
                let
                    maybeSelectedMission =
                        fmap snd . L.listSelectedElement $ asMissionList updatedState
                    runAsyncMission =
                        liftIO . async . maybe (return ())
                            (runMission (asLogEntriesMVar s) (asGlobalOptions s))
                in
                    if focusedSection == MissionSelect then
                        runAsyncMission maybeSelectedMission
                            >>= (\asyncProc -> continue updatedState { asMissionThread = Just asyncProc })
                    else
                        continue updatedState

            -- Quit
            V.EvKey (V.KChar 'q') [] ->
                halt updatedState
            V.EvKey V.KEsc [] ->
                halt updatedState

            -- Cycle through Sections
            V.EvKey (V.KChar 'n') [V.MCtrl] ->
                continue $ updatedState
                    { asFocusedSection = nextEnum focusedSection }
            V.EvKey (V.KChar 'p') [V.MCtrl] ->
                continue $ updatedState
                    { asFocusedSection = previousEnum focusedSection }
            _ ->
                continue updatedState
    _ ->
        continue s


runMission  :: MVar [LogEntry] -> TVar GlobalOptions -> Mission -> IO ()
runMission logMVar optionsTVar m =
    let
        logger =
            liftIO . messageLogger

        prog =
            case m of
                LowKerbinOrbit ->
                    lowKerbinOrbit
                ExecuteManeuver ->
                    executeManeuverProgram
                SubOrbital ->
                    subOrbital
                _ ->
                    fail "runMission: UNIMPLEMENTED MISSION"

        missionTitle = case m of
            LowKerbinOrbit ->
                "Low Kerbin Orbit"
            ExecuteManeuver ->
                "Maneuver Execution"
            SubOrbital ->
                "Sub-Orbital Launch"
            _ ->
                fail "runMission: UNIMPLEMENTED MISSION"
        run =
            withAutoStaging logger $
            withRPCClient (show m) rpcServer rpcPort $ \client ->
            withStreamClient client rpcServer rpcStreamPort $ \streamClient ->
                runRPCProg client $ do
                    logger . T.pack $ missionTitle ++ " Program Initiated."
                    prog logger streamClient
    in
        catch run errorHandler
    where
        rpcServer =
            "127.0.0.1"
        rpcPort =
            "50000"
        rpcStreamPort =
            "50001"
        messageLogger :: T.Text -> IO ()
        messageLogger msg = do
            newEntry <- makeLogEntry msg
            modifyMVar_ logMVar (return . (:) newEntry)
        errorHandler :: SomeException -> IO ()
        errorHandler e =
            messageLogger . T.pack $ "Mission Program Exception!\n" ++ show e
        withAutoStaging :: LoggingFunction -> IO () -> IO ()
        withAutoStaging logger p =
            bracket (forkIO $ runAutoStaging logger) killThread $ const p
        runAutoStaging :: LoggingFunction -> IO ()
        runAutoStaging logger =
            withRPCClient "Auto-Staging" rpcServer rpcPort
                $ flip runRPCProg
                $ autoStaging optionsTVar logger


nextEnum :: (Enum a, Bounded a, Eq a) => a -> a
nextEnum e =
    if e == maxBound then
        minBound
    else
        succ e

previousEnum :: (Enum p, Bounded p, Eq p) => p -> p
previousEnum e =
    if e == minBound then
        maxBound
    else
        pred e
