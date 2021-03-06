{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-| Utilities for automating Kerbal Space Program using the kRPC mod.

TODO Functionality:

* Parameterized Tasks(set LKO Apoapsis)
* Function to warp to & execute an manually placed maneuver node
* Function to match the inclination of a target
* Hoffman Transfer
* Rendezvous
* Docking
* Landing

TODO Tasks:

* KSC -> LKO -> Dock w/ KerbStation
* Minmus Surface -> Dock w/ MunStation -> Dump Fuel -> Land on Minmus
* Minmus -> Kerbin/Mun Fuel Runs

-}
module KSP.Automation where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, when, void, forever, unless)
import Control.Monad.IO.Class (liftIO)
import Data.Default (Default(def))
import Data.IORef (IORef, newIORef, modifyIORef, readIORef)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Monoid ((<>))
import GHC.Conc (TVar, readTVarIO)

import KRPCHS
import KRPCHS.SpaceCenter

import qualified Data.Map as M
import qualified Data.Text as T


-- | A function used by Automation programs to log messages.
type LoggingFunction = (T.Text -> RPCContext ())

-- | Log Messages to StdOut
logStdOut :: LoggingFunction
logStdOut =
    liftIO . putStrLn . T.unpack


newtype GlobalOptions
    = GlobalOptions
        { goEnableAutoStaging :: Bool
        }

instance Default GlobalOptions where
    def = GlobalOptions
        { goEnableAutoStaging = True
        }

-- Auto-Staging
newtype AutoStagingData
    = AutoStagingData
        { asdLastEnabledValue :: Bool }

autoStaging :: TVar GlobalOptions -> LoggingFunction -> RPCContext ()
autoStaging optionsTVar logMessage = do
    logMessage "Initializing Auto Staging Sub-Process."
    dataRef <- liftIO
        $ goEnableAutoStaging <$> readTVarIO optionsTVar >>= newIORef . AutoStagingData
    forever $ do
        isLaunched <- (/= VesselSituation'PreLaunch) <$> (getActiveVessel >>= getVesselSituation)
        enabled <- logAndUpdateEnabledStatus dataRef

        when (enabled && isLaunched) $ do
            v <- getActiveVessel
            ctrl <- getVesselControl v
            currentStage <- getControlCurrentStage ctrl
            stageDepleted <- stageResourcesDepleted v ctrl
            when (stageDepleted && currentStage /= 0) . void
                $ logMessage "Resources Empty, Decoupling Stage."
                >> controlActivateNextStage ctrl
        liftIO $ threadDelay 500000
    where
        logAndUpdateEnabledStatus :: IORef AutoStagingData -> RPCContext Bool
        logAndUpdateEnabledStatus dataRef = do
            enabled <- liftIO $ goEnableAutoStaging <$> readTVarIO optionsTVar
            lastEnabledValue <- liftIO $ asdLastEnabledValue <$> readIORef dataRef
            case (lastEnabledValue, enabled) of
                (True, False) ->
                    logMessage "Auto-Staging Disabled."
                (False, True) ->
                    logMessage "Auto-Staging Enabled."
                _ ->
                    return ()
            liftIO $ modifyIORef dataRef $ \asd -> asd { asdLastEnabledValue = enabled }
            return enabled
        stageResourcesDepleted :: Vessel -> Control -> RPCContext Bool
        stageResourcesDepleted v ctrl = do
            currentStage <- getControlCurrentStage ctrl
            resources <- vesselResourcesInDecoupleStage v currentStage False
            maxSolidFuel <- resourcesMax resources "SolidFuel"
            solidFuel <- resourcesAmount resources "SolidFuel"
            maxLiquidFuel <- resourcesMax resources "LiquidFuel"
            liquidFuel <- resourcesAmount resources "LiquidFuel"
            let solidLiquidStageDepleted =
                    solidFuel == 0 && maxSolidFuel > 0 && liquidFuel == 0 && maxLiquidFuel > 0
                solidOnlyStageDepleted =
                    solidFuel == 0 && maxSolidFuel > 0 && maxLiquidFuel == 0
                liquidOnlyStageDepleted =
                    liquidFuel == 0 && maxLiquidFuel > 0 && maxSolidFuel == 0
                currentStageDepleted =
                    solidLiquidStageDepleted || solidOnlyStageDepleted || liquidOnlyStageDepleted
                nextStage =
                    currentStage - 1

            nextResources <- vesselResourcesInDecoupleStage v nextStage False
            nextSolidFuel <- resourcesMax nextResources "SolidFuel"
            nextLiquidFuel <- resourcesMax nextResources "LiquidFuel"
            let nextStageHasFuel =
                    nextLiquidFuel > 0 || nextSolidFuel > 0
            return $ currentStageDepleted && nextStageHasFuel




-- Sub-Orbital

data SubOrbitalData
    = SubOrbitalData
        { sodVerticalVelocity :: Double
        , experimentsRun :: M.Map T.Text Integer    -- ^ Subject Title --> Run Count
        }

subOrbital :: LoggingFunction -> StreamClient -> RPCContext ()
subOrbital logMessage streamClient = do
    v <- initializeVesselData logMessage
    ctrl <- getVesselControl v
    experiments <- getVesselParts v >>= getPartsExperiments

    surfaceReferenceFrame <- getVesselOrbit v
        >>= getOrbitBody
        >>= getCelestialBodyReferenceFrame
    flight <- vesselFlight v surfaceReferenceFrame

    dataRef <- liftIO . newIORef $ SubOrbitalData 9001 M.empty
    velocityStream <- getFlightVerticalSpeedStream flight

    launch logMessage v (90, 90, 90)

    withPitchTable streamClient launchpadPitchAngles v (isFalling velocityStream dataRef) $
        forM_ experiments $ \experiment -> do
            isAvailable <- getExperimentAvailable experiment
            hasData <- getExperimentHasData experiment
            subject <- getExperimentScienceSubject experiment
            subjectTitle <- getScienceSubjectTitle subject
            isComplete <- getScienceSubjectIsComplete subject
            runCount <- fromMaybe 0 . M.lookup subjectTitle . experimentsRun
                <$> liftIO (readIORef dataRef)
            when (isAvailable && not hasData && not isComplete && runCount < 3) $ do
                logMessage $ "Collecting Science: " <> subjectTitle <> "."
                experimentRun experiment
                liftIO . modifyIORef dataRef $ \sd -> sd
                    { experimentsRun =
                        M.alter
                            (\case
                                Nothing ->
                                    Just 1
                                Just a ->
                                    Just $ a + 1
                            ) subjectTitle $ experimentsRun sd
                    }
                liftIO $ threadDelay 1000000

    -- TODO: Warp to ~40km
    -- TODO: Align Retrograde
    -- TODO: Stage Until Parachutes are Deployed
    logMessage "Activating Parachutes."
    loop $ do
        parachutes <- getVesselParts v >>= getPartsParachutes
        parachutesDeployed <- and <$> mapM getParachuteDeployed parachutes
        unless parachutesDeployed
            . void $ controlActivateNextStage ctrl
        return parachutesDeployed
    -- TODO: Warp to 1 second before touchdown.
    where
        isFalling velocityStream dataRef msg _ = do
            when (messageHasResultFor velocityStream msg) $ do
                verticalVelocity <- getStreamResult velocityStream msg
                liftIO . modifyIORef dataRef
                    $ \c -> c { sodVerticalVelocity = verticalVelocity }
            (< 0) . sodVerticalVelocity <$> liftIO (readIORef dataRef)




-- Execute Maneuver

-- | A Program for executing a maneuver node. A basic wrapper around the
-- `executeManeuver` sub-routine.
executeManeuverProgram :: LoggingFunction -> StreamClient -> RPCContext ()
executeManeuverProgram logMessage streamClient = do
    v <- initializeVesselData logMessage
    ctrl <- getVesselControl v
    listToMaybe <$> getControlNodes ctrl >>= \case
        Nothing ->
            logMessage "Could Not Find A Maneuver Node - Aborting."
        Just node -> do
            executeManeuver logMessage streamClient v node
            enableStabilityAssist ctrl


-- | A sub-routine for executing a maneuver node.
--
-- TODO: PID Controller for throttle control
executeManeuver :: LoggingFunction -> StreamClient -> Vessel -> Node -> RPCContext ()
executeManeuver logMessage streamClient v node = do
    pilot <- getVesselAutoPilot v
    ctrl <- getVesselControl v

    logMessage "Calculating Burn Time for Maneuver."
    -- TODO: Eventually, should account for having to decouple during a burn
    deltaV <- getNodeDeltaV node
    maxThrust <- realToFrac <$> getVesselAvailableThrust v
    isp <- (* 9.82) . realToFrac <$> getVesselSpecificImpulse v
    m0 <- realToFrac <$> getVesselMass v
    let m1 = m0 / exp (deltaV / isp)
        flowRate = maxThrust / isp
        burnTime = (m0 - m1) / flowRate
    logMessage . T.pack $ "Calculated Burn Time of " ++ show (round burnTime :: Integer) ++ "s."

    logMessage "Warping to Approximate Burn Time."
    nodeUT <- getNodeUT node
    warpTo (nodeUT - (burnTime / 2)) 100000 2
    logMessage "Warp Completed."

    logMessage "Orienting Vessel Towards Burn Direction."
    nodeFrame <- getNodeReferenceFrame node
    autoPilotEngage pilot
    setAutoPilotReferenceFrame pilot nodeFrame
    setAutoPilotTargetDirection pilot (0, 1, 0)
    logMessage "Vessel Re-Orientation Complete."

    logMessage "Waiting for Burn Point."
    utStream <- getUTStream
    loop $ do
        msg <- getStreamMessage streamClient
        if messageHasResultFor utStream msg then do
            ut <- getStreamResult utStream msg
            return $ ut >= (nodeUT - (burnTime / 2))
        else
            return False

    logMessage "Beginning Maneuver Burn."
    burnVectorStream <- nodeRemainingBurnVectorStream node nodeFrame
    burnVectorRef <- liftIO $ newIORef (0, 9001, 0)
    loop $ do
        msg <- getStreamMessage streamClient
        when (messageHasResultFor burnVectorStream msg) $
            getStreamResult burnVectorStream msg
            >>= void . liftIO . modifyIORef burnVectorRef . const
        burnVector <- liftIO (readIORef burnVectorRef)
        setAutoPilotTargetDirection pilot burnVector
        let remainingBurn = mag burnVector
        -- TODO: PID Controller!
        when (remainingBurn >= 20) $ setControlThrottle ctrl 1
        when (remainingBurn < 20 && remainingBurn >= 5) $ setControlThrottle ctrl 0.5
        when (remainingBurn < 5) $ setControlThrottle ctrl 0.25
        return $ remainingBurn < 0.1
    setControlThrottle ctrl 0
    logMessage "Maneuver Completed."
    autoPilotDisengage pilot



-- Low Kerbin Orbit

-- | An automated routine for taking off & attempting to achieve
-- a circular Low Kerbin Orbit(75,000m).
--
-- This is incomplete and has only been tested on a handful of rocket.
--
-- It starts by firing SRBs & pitching down when various surface velocities
-- & apoapses are reached.
--
-- When the SRBs are depleted it will activate the LF stage and maintain
-- a TWR of ~2.5 until the target apoapsis height is achieved. Then
-- a circularizing maneuver node is planned and executed.
--
-- TODO: Eventually attempt to match inclination w/ a target?
lowKerbinOrbit :: LoggingFunction -> StreamClient -> RPCContext ()
lowKerbinOrbit logMessage streamClient = do
    v <- initializeVesselData logMessage
    ctrl <- getVesselControl v
    liftOff logMessage streamClient v
    setControlSAS ctrl True >> setControlSASMode ctrl SASMode'Prograde
    circularizeOrbit logMessage streamClient v
    enableStabilityAssist ctrl
    logMessage "Low Kerbin Orbit Achieved."


-- | Used to store the last data received from `liftOff`'s RPC streams.
data LiftOffData
    = LiftOffData
        { lodAltitude :: Double -- ^ Flight's Mean Altitude
        , lodThrust :: Float    -- ^ Vessel's Current Thrust
        }

-- | Lift-off from the KSC Launchpad & attain an apoapsis of 75km.
liftOff :: LoggingFunction -> StreamClient -> Vessel -> RPCContext ()
liftOff logMessage streamClient v = do
    logMessage "Initializing Lift-Off Data & Telemetry Streams."
    pilot <- getVesselAutoPilot v
    ctrl <- getVesselControl v

    dataRef <- liftIO . newIORef $ LiftOffData 0 0
    thrustStream <- getVesselThrustStream v

    launch logMessage v (90, 90, 90)

    logMessage "Raising Apoapsis to 75km."
    withPitchTable streamClient launchpadPitchAngles v reachedApoapsis $ do
        msg <- getStreamMessage streamClient
        when (messageHasResultFor thrustStream msg) $ do
            thrust <- getStreamResult thrustStream msg
            liftIO . modifyIORef dataRef $ \c -> c { lodThrust = thrust }
        -- Adjust Throttle to Maintain 2.5 TWR
        thrust <- liftIO $ lodThrust <$> readIORef dataRef
        weight <- lodAltitude <$> liftIO (readIORef dataRef) >>= weightAtAltitude v
        let twr = thrust / weight
        -- TODO: Fine-tune TWR range using Altitude/Atmosphere Density
        -- TODO: Smooth out changes, very jumpty ATM(see PID Controllers)
        when (twr < 2.3) $
            getControlThrottle ctrl
            >>= (\t -> setControlThrottle ctrl . max 1 $ t + 0.0005)
        when (twr > 2.7) $
            getControlThrottle ctrl
            >>= (\t -> setControlThrottle ctrl . min 0 $ t - 0.0005)

    logMessage "Desired Apoapsis Altitude Achieved, Disabling Throttle."
    setControlThrottle ctrl 0
    autoPilotDisengage pilot
    where
        reachedApoapsis _ data_ =
            return . (> 75000) $ ptdApoapsis data_


-- | Circularize the current orbit at it's apoapsis.
circularizeOrbit :: LoggingFunction -> StreamClient -> Vessel -> RPCContext ()
circularizeOrbit logMessage streamClient v = do
    ctrl <- getVesselControl v

    -- Calculate DeltaV Required for Circularization, Add Maneuver Node
    logMessage "Calculating DeltaV & Burn Time for Circularization."
    (mu, r, a1) <- getVesselOrbit v >>= \orbit ->
        (,,)
            <$> (getOrbitBody orbit >>= getCelestialBodyGravitationalParameter)
            <*> (realToFrac <$> getOrbitApoapsis orbit)
            <*> (realToFrac <$> getOrbitSemiMajorAxis orbit)
    let a2 = r
        v1 = sqrt $ mu * ((2 / r) - (1 / a1))
        v2 = sqrt $ mu * ((2 / r) - (1 / a2))
        deltaV = (v2 - v1) * 1.05
    nodeUT <- getUT
    timeToApoapsis_ <- getVesselOrbit v >>= getOrbitTimeToApoapsis
    node <- controlAddNode ctrl (nodeUT + timeToApoapsis_) deltaV 0 0
    logMessage "Added Circularization Maneuver Node."

    executeManeuver logMessage streamClient v node

    logMessage "Removing Circularization Maneuver Node."
    nodeRemove node



-- Pitch Tables

type PitchTable
    = [ ( ( Velocity, Apoapsis ), Pitch ) ]

type Velocity
    = Double
type Apoapsis
    = Double
type Pitch
    = Float

launchpadPitchAngles :: PitchTable
launchpadPitchAngles =
            [ ( ( 0, 0 ), 90 )
            , ( ( 100, 6500 ), 80 )
            , ( ( 250, 10000 ), 65 )
            , ( ( 450, 15000 ), 55 )
            , ( ( 500, 30000 ), 45 )
            , ( ( 600, 45000 ), 20 )
            , ( ( 700, 60000 ), 10 )
            , ( ( 800, 70000 ), 0 )
            ]


data PitchTableData
    = PitchTableData
        { ptdVelocity :: Double
        , ptdAltitude :: Double
        , ptdApoapsis :: Double
        }

-- | Apply a Pitch Table to the Current Vessel, Run Some Action, & Abort
-- When Some Condition is Satisfied.
--
-- TODO: Smooth out pitch changes, PID Controller?
withPitchTable :: StreamClient -> PitchTable -> Vessel -> (KRPCStreamMsg -> PitchTableData -> RPCContext Bool) -> RPCContext () -> RPCContext ()
withPitchTable streamClient angles v shouldStop action = do
    pilot <- getVesselAutoPilot v
    surfaceReferenceFrame <- getVesselOrbit v
        >>= getOrbitBody
        >>= getCelestialBodyReferenceFrame
    flight <- vesselFlight v surfaceReferenceFrame

    dataRef <- liftIO . newIORef $ PitchTableData 0 0 0

    altitudeStream <- getFlightMeanAltitudeStream flight
    velocityStream <- vesselVelocityStream v surfaceReferenceFrame
    apoapsisStream <- getVesselOrbit v >>= getOrbitApoapsisAltitudeStream
    loop $ do
        -- TODO: Use StateT instead of IORef?
        let update = liftIO . modifyIORef dataRef
        -- Update the Data Using Message Streams
        msg <- getStreamMessage streamClient
        when (messageHasResultFor altitudeStream msg) $ do
            altitude <- getStreamResult altitudeStream msg
            update $ \c -> c { ptdAltitude = altitude }
        when (messageHasResultFor velocityStream msg) $ do
            velocity <- mag <$> getStreamResult velocityStream msg
            update $ \c -> c { ptdVelocity = velocity }
        when (messageHasResultFor apoapsisStream msg) $ do
            apoapsis <- getStreamResult apoapsisStream msg
            update $ \c -> c { ptdApoapsis = apoapsis }

        liftIO (readIORef dataRef) >>= updatePitch pilot

        action

        shouldStop msg =<< liftIO (readIORef dataRef)
    where
        updatePitch :: AutoPilot -> PitchTableData -> RPCContext ()
        updatePitch pilot data_ =
            let
                velocity =
                    ptdVelocity data_
                apoapsis =
                    ptdApoapsis data_
            in
                loopMatch angles (\(v', a) -> v' < velocity && a >= apoapsis)
                    (setAutoPilotTargetPitch pilot)



-- UTILTIES

-- | Log a Messge & Grab the Active Vessel Data.
initializeVesselData :: LoggingFunction -> RPCContext Vessel
initializeVesselData logMessage =
    logMessage "Initializing Vessel Data." >> getActiveVessel

-- | Launch the Vessel with the Given Pitch, Heading, & Roll.
--
-- Enables & configures the Autopilot, Maximizes the Throttle, & Activates
-- the First Stage.
launch :: LoggingFunction -> Vessel -> (Float, Float, Float) -> RPCContext ()
launch logMessage v (pitch, heading, roll) = do
    pilot <- getVesselAutoPilot v
    ctrl <- getVesselControl v

    logMessage "Setting Launch Parameters."
    autoPilotTargetPitchAndHeading pilot pitch heading
    setAutoPilotTargetRoll pilot roll
    autoPilotEngage pilot
    setControlThrottle ctrl 1

    countdown logMessage 3
    logMessage "Launch!"
    void $ controlActivateNextStage ctrl

-- | Calculate a `Vessel`'s weight at a specific altitude of the
-- `CelestialBody` the vessel is orbiting.
weightAtAltitude :: Vessel -> Double -> RPCContext Float
weightAtAltitude v altitude = do
        vesselMass <- getVesselMass v
        (surfaceGravity, bodyRadius) <-
           getVesselOrbit v >>= getOrbitBody >>= \body ->
            (,) <$> getCelestialBodySurfaceGravity body
                <*> getCelestialBodyEquatorialRadius body
        let surfaceWeight =
                surfaceGravity * vesselMass
            gravityRatio =
                (bodyRadius / (bodyRadius + realToFrac altitude)) ^^ (2 :: Integer)
        return $ surfaceWeight * gravityRatio


-- | Enable SAS & set it to Stability Assist Mode. Cannot be used while
-- autopilot is enabled.
enableStabilityAssist :: Control -> RPCContext ()
enableStabilityAssist ctrl =
    setControlSAS ctrl True >> setControlSASMode ctrl SASMode'StabilityAssist


-- | Determine the Magnitude of a Vector.
mag :: (Double, Double, Double) -> Double
mag (x, y, z) =
    sqrt $ x ^^ (2 :: Integer) + y ^^ (2 :: Integer) + z ^^ (2 :: Integer)


-- | Log a countdown.
countdown :: LoggingFunction -> Int -> RPCContext ()
countdown logMessage n =
    forM_ (reverse [1 .. n]) $ \sec ->
        logMessage (T.pack $ show sec) >> liftIO (threadDelay oneSecond)


-- | Loop an action until it returns True.
loop :: Monad m => m Bool -> m ()
loop a =
    a >>= \b -> if b then return () else loop a


-- | Loop through a list until a match is found, then use it to run an
-- action.
loopMatch :: Monad m => [(a, b)] -> (a -> Bool) -> (b -> m ()) -> m ()
loopMatch matches p f =
    case matches of
        [] ->
            return ()
        (k, v) : rest ->
            if p k then
                f v
            else
                loopMatch rest p f


-- | The `Int` representation of a second for the `threadDelay` function.
--
-- This lets you do things like:
--
-- >>> threadDelay (3 * oneSecond) >>= print    -- Wait 3 Seconds
-- ()
oneSecond :: Int
oneSecond =
    1000000
