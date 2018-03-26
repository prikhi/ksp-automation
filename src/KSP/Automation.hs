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
import Control.Monad (forM_, when, void)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (newIORef, modifyIORef, readIORef)

import KRPCHS
import KRPCHS.SpaceCenter

import qualified Data.Text as T


-- | A function used by Automation programs to log messages.
type LoggingFunction = (T.Text -> RPCContext ())

-- | Log Messages to StdOut
logStdOut :: LoggingFunction
logStdOut =
    liftIO . putStrLn . T.unpack


-- | Used to store the last data received from `liftOff`'s RPC streams.
data LiftOffData
    = LiftOffData
        { lodVelocity :: Double -- ^ Vessel's Surface Velocity
        , lodAltitude :: Double -- ^ Flight's Mean Altitude
        , lodApoapsis :: Double -- ^ Orbit's Apoapsis Altitude
        , lodThrust :: Float    -- ^ Vessel's Current Thrust
        }

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


-- | A sub-routine for executing a manuever node.
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
    logMessage . T.pack $ "Calculated Burn Time of " ++ show (round burnTime :: Integer) ++ "s"

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
-- a circularizing manuever node is planned and executed.
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


-- | Lift-off from the KSC Launchpad & attain an apoapsis of 75km.
liftOff :: LoggingFunction -> StreamClient -> Vessel -> RPCContext ()
liftOff logMessage streamClient v = do
    logMessage "Initializing Lift-Off Data & Telemetry Streams."
    pilot <- getVesselAutoPilot v
    ctrl <- getVesselControl v
    body <- getVesselOrbit v >>= getOrbitBody
    surfaceReferenceFrame <- getCelestialBodyReferenceFrame body
    flight <- vesselFlight v surfaceReferenceFrame

    dataRef <- liftIO . newIORef $ LiftOffData 0 0 0 0
    altitudeStream <- getFlightMeanAltitudeStream flight
    velocityStream <- vesselVelocityStream v surfaceReferenceFrame
    thrustStream <- getVesselThrustStream v
    apoapsisStream <- getVesselOrbit v >>= getOrbitApoapsisAltitudeStream

    logMessage "Setting Launch Parameters."
    setAutoPilotSAS pilot True
    setAutoPilotSASMode pilot SASMode'StabilityAssist
    autoPilotTargetPitchAndHeading pilot 90 90
    setAutoPilotTargetRoll pilot 90
    autoPilotEngage pilot
    setControlThrottle ctrl 1

    countdown logMessage 3
    logMessage "Launch!"
    void $ controlActivateNextStage ctrl

    logMessage "Raising Apoapsis to 75km."
    loop $ do
        -- TODO: Use StateT instead of IORef
        let update = liftIO . modifyIORef dataRef
        -- Update the Data Using Message Streams
        msg <- getStreamMessage streamClient
        when (messageHasResultFor altitudeStream msg) $ do
            altitude <- getStreamResult altitudeStream msg
            update $ \c -> c { lodAltitude = altitude }
        when (messageHasResultFor velocityStream msg) $ do
            velocity <- mag <$> getStreamResult velocityStream msg
            update $ \c -> c { lodVelocity = velocity }
        when (messageHasResultFor thrustStream msg) $ do
            thrust <- getStreamResult thrustStream msg
            update $ \c -> c { lodThrust = thrust }
        when (messageHasResultFor apoapsisStream msg) $ do
            apoapsis <- getStreamResult apoapsisStream msg
            update $ \c -> c { lodApoapsis = apoapsis }

        -- Control Pitch Angle
        liftIO (readIORef dataRef) >>= updatePitch pilot

        -- Decouple SRBs when Empty
        currentStage <- getControlCurrentStage ctrl
        resources <- vesselResourcesInDecoupleStage v currentStage False
        solidFuel <- resourcesAmount resources "SolidFuel"
        liquidFuel <- resourcesAmount resources "LiquidFuel"
        when (solidFuel == 0 && liquidFuel == 0) . void
            $ logMessage "Resources Empty, Decoupling Stage."
            >> controlActivateNextStage ctrl

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

        -- Stop at Apoapsis of 75km
        (> 75000) . lodApoapsis <$> liftIO (readIORef dataRef)

    logMessage "Desired Apoapsis Altitude Achieved, Disabling Throttle."
    setControlThrottle ctrl 0
    autoPilotDisengage pilot
    where
        -- TODO: Smooth out pitch changes, PID Controller?
        updatePitch pilot data_ =
            let
                angles =
                    [ ( ( 0, 0 ), 90 )
                    , ( ( 100, 6500 ), 80 )
                    , ( ( 250, 10000 ), 65 )
                    , ( ( 450, 15000 ), 55 )
                    , ( ( 500, 30000 ), 45 )
                    , ( ( 600, 45000 ), 20 )
                    , ( ( 800, 65000 ), 0 )
                    ]
                velocity = lodVelocity data_
                apoapsis = lodApoapsis data_
            in
                loopMatch angles (\(v', a) -> v' < velocity && a >= apoapsis) (setAutoPilotTargetPitch pilot)


-- | Circularize the current orbit at it's apoapsis.
circularizeOrbit :: LoggingFunction -> StreamClient -> Vessel -> RPCContext ()
circularizeOrbit logMessage streamClient v = do
    ctrl <- getVesselControl v

    -- Calculate DeltaV Required for Circularization, Add Manuever Node
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
    logMessage "Added Circularization Manuever Node."

    executeManeuver logMessage streamClient v node

    logMessage "Removing Circularization Maneuver Node."
    nodeRemove node


-- UTILTIES

-- | Log a Messge & Grab the Active Vessel Data.
initializeVesselData :: LoggingFunction -> RPCContext Vessel
initializeVesselData logMessage =
    logMessage "Initializing Vessel Data." >> getActiveVessel

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
