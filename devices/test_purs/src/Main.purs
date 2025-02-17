module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Time.Duration (Milliseconds(..))
import Data.DateTime.Instant (unInstant)
import Control.Monad.Free (Free, liftF, runFreeM)
import Effect (Effect)
import Effect.Now as EffectNow
import Effect.Laff (Laff, delay, runLaff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Random (random, randomInt)
import Record as R
import Simple.JSON (writeJSON)

data State
  = InitialState
  | ReceivingState ReceivingStateData
  | ReceivingPauseState ReceivingStateData
  | DeepSleepingState DeepSleepingStateData

type ReceivingStateData = { socket :: Socket, timeLastSendBatteryVoltage :: Milliseconds }

type DeepSleepingStateData = { socket :: Maybe Socket }

type ConnectionConfig
  = { timeout :: Milliseconds
    , host :: String
    , port :: Int
    , wifi_ssid :: String
    , wifi_password :: String
    }

type Config
  = { connectionConfig :: ConnectionConfig
    , loopSleepPeriod :: Milliseconds
    , batteryVoltageSendPeriod :: Milliseconds
    , deepSleepPeriod :: Milliseconds
    }

data SocketMessage
  = SocketMessageReadSensor
  | SocketMessageReadBatteryVoltage
  | SocketMessageFlashLights
  | SocketMessageDoThingA
  | SocketMessageDoThingB
  | SocketMessageStoreFile String Int
  | SocketMessageReboot

derive instance Generic SocketMessage _
instance Show SocketMessage where
  show = genericShow

data SystemMessage
  = SystemMessageSocketClosed
  | SystemMessageAction (StateLangF Unit)

instance Show SystemMessage where
  show SystemMessageSocketClosed = "SystemMessageSocketClosed"
  show (SystemMessageAction _) = "SystemMessageAction"

data StateLangF a
  -- = ToJson _ (String -> a)
  = Now (Milliseconds -> a)
  | Sleep Milliseconds a
  | DeepSleep Milliseconds a
  | DoAfter Milliseconds (StateLangF Unit) a
  | ReadBatteryVoltage (Maybe Number -> a)
  | Connect ConnectionConfig (Maybe Socket -> a)
  | CloseConnection Socket a
  | SendSocketMessage Socket String a
  | ReceiveSocketMessage Socket (Maybe SocketMessage -> a)
  | ReceiveSystemMessage (Maybe SystemMessage -> a)
  | StoreFile String Int a
  | ReadSensor (Maybe Number -> a)
  | DoThingA a
  | DoThingB a
  | FlashLights a

instance functorStateLangF :: Functor StateLangF where
  --map f (ToJson d k) = ToJson d (f <<< k)
  map f (Now k) = Now (f <<< k)
  map f (Sleep millis a) = Sleep millis (f a)
  map f (DeepSleep millis a) = DeepSleep millis (f a)
  map f (DoAfter millis action a) = DoAfter millis action (f a)
  map f (ReadBatteryVoltage k) = ReadBatteryVoltage (f <<< k)
  map f (Connect config k) = Connect config (f <<< k)
  map f (CloseConnection socket a) = CloseConnection socket (f a)
  map f (SendSocketMessage socket message a) = SendSocketMessage socket message (f a)
  map f (ReceiveSocketMessage socket k) = ReceiveSocketMessage socket (f <<< k)
  map f (ReceiveSystemMessage k) = ReceiveSystemMessage (f <<< k)
  map f (StoreFile name size a) = StoreFile name size (f a)
  map f (ReadSensor k) = ReadSensor (f <<< k)
  map f (DoThingA a) = DoThingA (f a)
  map f (DoThingB a) = DoThingB (f a)
  map f (FlashLights a) = FlashLights (f a)

type StateLang = Free StateLangF

--toJson :: _ -> StateLang String
--toJson asdf = liftF $ ToJson asdf id

now :: StateLang Milliseconds
now = liftF $ Now identity

sleep :: Milliseconds -> StateLang Unit
sleep millis = liftF $ Sleep millis unit

deepSleep :: Milliseconds -> StateLang Unit
deepSleep millis = liftF $ DeepSleep millis unit

doAfter :: Milliseconds -> StateLangF Unit -> StateLang Unit
doAfter millis action = liftF $ DoAfter millis action unit

readBatteryVoltage :: StateLang (Maybe Number)
readBatteryVoltage = liftF $ ReadBatteryVoltage identity

connect :: ConnectionConfig -> StateLang (Maybe Socket)
connect connectionConfig = liftF $ Connect connectionConfig identity

closeConnection :: Socket -> StateLang Unit
closeConnection socket = liftF $ CloseConnection socket unit

sendSocketMessage :: Socket -> String -> StateLang Unit
sendSocketMessage socket message = liftF $ SendSocketMessage socket message unit

receiveSocketMessage :: Socket -> StateLang (Maybe SocketMessage)
receiveSocketMessage socket = liftF $ ReceiveSocketMessage socket identity

receiveSystemMessage :: StateLang (Maybe SystemMessage)
receiveSystemMessage = liftF $ ReceiveSystemMessage identity

storeFile :: String -> Int -> StateLang Unit
storeFile name size = liftF $ StoreFile name size unit

readSensor :: StateLang (Maybe Number)
readSensor = liftF $ ReadSensor identity

doThingA :: StateLang Unit
doThingA = liftF $ DoThingA unit

doThingB :: StateLang Unit
doThingB = liftF $ DoThingB unit

flashLights :: StateLang Unit
flashLights = liftF $ FlashLights unit

program :: Config -> (State -> StateLang State)
program config =
  let
    handleSystemMessage :: ReceivingStateData -> SystemMessage -> StateLang State
    handleSystemMessage _ SystemMessageSocketClosed = pure $ DeepSleepingState {socket: Nothing}
    handleSystemMessage stateData (SystemMessageAction action) = do
      liftF action
      pure $ ReceivingState stateData

    handleSocketMessage :: ReceivingStateData -> SocketMessage -> StateLang State
    handleSocketMessage stateData (SocketMessageStoreFile name size) = do
      storeFile name size
      pure $ ReceivingState stateData
    handleSocketMessage _ SocketMessageReboot = do
      deepSleep $ Milliseconds 0.0
      pure InitialState
    handleSocketMessage stateData SocketMessageReadSensor = do
      readSensor >>= case _ of
        Nothing -> pure unit
        Just sensorReading -> sendSocketMessage state.socket $ writeJSON {sensorReading: sensorReading}
      pure $ ReceivingState stateData
    handleSocketMessage stateData SocketMessageReadBatteryVoltage = do
      readBatteryVoltage >>= case _ of
        Nothing -> pure unit
        Just voltage -> sendSocketMessage stateData.socket $ writeJSON {batteryVoltage: voltage}
      pure $ ReceivingState stateData
    handleSocketMessage stateData SocketMessageFlashLights = do
      flashLights
      pure $ ReceivingState stateData
    handleSocketMessage stateData SocketMessageDoThingA = do
      doThingA
      pure $ ReceivingState stateData
    handleSocketMessage stateData SocketMessageDoThingB = do
      doThingB
      pure $ ReceivingState stateData
  in
    case _ of
      InitialState ->
        readBatteryVoltage >>= case _ of
          Just batteryVoltage | batteryVoltage < 3.3 ->
            pure $ DeepSleepingState {socket: Nothing}
          _ -> do
              connect config.connectionConfig >>= case _ of
                Nothing -> pure $ DeepSleepingState {socket: Nothing}
                Just socket -> pure $ ReceivingState { timeLastSendBatteryVoltage: Milliseconds 0.0, socket : socket }
      ReceivingState stateData -> do
        fromMaybe (pure $ ReceivingPauseState stateData) =<< (runMaybeT $
          (MaybeT $ receiveSystemMessage <#> map (handleSystemMessage stateData))
          <|>
          (MaybeT $ receiveSocketMessage stateData.socket <#> map (handleSocketMessage stateData))
          <|>
          (MaybeT $ readBatteryVoltage <#> map \batteryVoltage -> do
             nowMillis <- now
             if nowMillis > stateData.timeLastSendBatteryVoltage <> config.batteryVoltageSendPeriod
               then sendSocketMessage stateData.socket $ writeJSON {batteryVoltage: batteryVoltage}
               else pure unit
             if batteryVoltage < 3.3
               then pure $ DeepSleepingState {socket: Just stateData.socket}
               else pure $ ReceivingPauseState $ stateData {timeLastSendBatteryVoltage = nowMillis}))
      ReceivingPauseState state -> do
        sleep config.loopSleepPeriod
        pure $ ReceivingState stateData
      DeepSleepingState stateData -> do
        case stateData.socket of
          Nothing -> pure unit
          Just socket -> closeConnection socket
        deepSleep config.deepSleepPeriod
        pure InitialState

runProgram :: (forall a. StateLangF a -> Laff a) -> State -> Laff Unit
runProgram interpreter s =
  let programLoopStep = program { loopSleepPeriod: Milliseconds 3000.0
                                , batteryVoltageSendPeriod: Milliseconds 500.0
                                , deepSleepPeriod: Milliseconds 4000.0
                                , connectionConfig:
                                  { timeout: Milliseconds 5000.0
                                  --, host: "localhost"
                                  , host: "192.168.0.68"
                                  , port: 9222
                                  , wifi_ssid: "hello"
                                  , wifi_password: "hi"
                                  }
                                }
                                s
  in do
    s' <- runFreeM interpreter programLoopStep
    runProgram interpreter s'

foreign import dummySocket :: Socket

interpretPrinty :: forall a. StateLangF a -> Laff a
--interpretPrinty (ToJson input k) =
--  pure $ k $ "<JSON payload " <> show input <> " >"
interpretPrinty (Now k) = do
  nowMillis <- liftEffect $ EffectNow.now
  pure $ k $ unInstant nowMillis
interpretPrinty (Sleep millis a) = do
  liftEffect $ Console.log ("sleeping for " <> show millis)
  liftEffect $ Console.log ""
  delay millis
  pure a
interpretPrinty (DeepSleep millis a) = do
  liftEffect $ Console.log ("deep-sleeping for " <> show millis)
  liftEffect $ Console.log ""
  delay millis
  pure a
interpretPrinty (DoAfter millis action a) = do
  liftEffect $ runLaff do
    delay millis
    interpretPrinty action
  pure a
interpretPrinty (ReadBatteryVoltage k) = do
  reading <- liftEffect do
    voltage <- random <#> \r -> r * 3.3 + 3.0
    choice <- randomInt 0 4
    pure if choice == 0 then Nothing else Just voltage
  liftEffect $ Console.log $ "read battery voltage: " <> show reading
  pure (k reading)
interpretPrinty (Connect _ k) = do
  socket <- liftEffect $ randomInt 0 4 <#> \r -> if r == 0 then Nothing else Just dummySocket
  liftEffect $ Console.log $ "connection created: " <> show (isJust socket)
  pure (k socket)
interpretPrinty (CloseConnection _ a) = do
  liftEffect $ Console.log "close connection"
  pure a
interpretPrinty (SendSocketMessage _ message a) = do
  liftEffect $ Console.log $ "send message " <> message
  pure a
interpretPrinty (ReceiveSocketMessage _ k) = do
  message <- liftEffect $ randomInt 0 (7 - 1 + 10) <#> case _ of
    0 -> Just $ SocketMessageStoreFile ".boot0" 55
    1 -> Just SocketMessageReboot
    2 -> Just SocketMessageReadSensor
    3 -> Just SocketMessageReadBatteryVoltage
    4 -> Just SocketMessageFlashLights
    5 -> Just SocketMessageDoThingA
    6 -> Just SocketMessageDoThingB
    _ -> Nothing
  liftEffect $ Console.log $ "received socket message: " <> show message
  pure $ k message
interpretPrinty (ReceiveSystemMessage k) = do
  message <- liftEffect $ randomInt 0 5 <#> case _ of
    0 -> Just SystemMessageSocketClosed
    1 -> Just $ SystemMessageAction (DoThingA unit)
    _ -> Nothing
  liftEffect $ Console.log $ "received system message: " <> show message
  pure $ k message
interpretPrinty (StoreFile name size a) = do
  liftEffect $ Console.log $ "Storing file with name " <> name <> " and size " <> show size
  pure a
interpretPrinty (ReadSensor k) = do
  reading <- liftEffect do
    value <- random <#> \r -> r * 5.0
    choice <- randomInt 0 (7 - 1 + 10)
    pure if choice == 0 then Nothing else Just value
  liftEffect $ Console.log $ "sensor reading " <> show reading
  pure $ k reading
interpretPrinty (DoThingA a) = do
  liftEffect $ Console.log "do thing a"
  pure a
interpretPrinty (DoThingB a) = do
  liftEffect $ Console.log "do thing b"
  pure a
interpretPrinty (FlashLights a) = do
  liftEffect $ Console.log "Flashing lights"
  pure a

foreign import data Socket :: Type

foreign import socketConnectImplJs ::
  (forall x. x -> Maybe x) -> (forall x. Maybe x) -> SystemMessage -> ConnectionConfig -> Laff (Maybe Socket)

socketConnectImpl :: ConnectionConfig -> Laff (Maybe Socket)
socketConnectImpl = socketConnectImplJs Just Nothing SystemMessageSocketClosed

foreign import socketCloseImpl :: Socket -> Effect Unit
foreign import socketWriteMessageImpl :: Socket -> String -> Laff Unit
foreign import socketReceiveImplJs ::
  (forall x. x -> Maybe x) ->
  (forall x. Maybe x) ->
  (String -> Int -> SocketMessage) ->
  SocketMessage ->
  SocketMessage ->
  SocketMessage ->
  SocketMessage ->
  SocketMessage ->
  SocketMessage ->
  Socket -> Laff (Maybe SocketMessage)

socketReceiveImpl :: Socket -> Laff (Maybe SocketMessage)
socketReceiveImpl = socketReceiveImplJs
  Just Nothing
  SocketMessageStoreFile
  SocketMessageReboot
  SocketMessageReadSensor
  SocketMessageReadBatteryVoltage
  SocketMessageFlashLights
  SocketMessageDoThingA
  SocketMessageDoThingB

foreign import systemReceiveImplJs :: (forall x. x -> Maybe x) -> (forall x. Maybe x) -> Laff (Maybe SystemMessage)

systemReceiveImpl :: Laff (Maybe SystemMessage)
systemReceiveImpl = systemReceiveImplJs Just Nothing

foreign import deepSleepImplJs :: Number -> Laff Unit

deepSleepImpl :: Milliseconds -> Laff Unit
deepSleepImpl (Milliseconds millis) = deepSleepImplJs millis

interpretSimNode :: forall a. StateLangF a -> Laff a
interpretSimNode (DeepSleep millis a) = do
  liftEffect $ Console.log ("deep-sleeping for " <> show millis)
  liftEffect $ Console.log ""
  deepSleepImpl millis
  pure a
interpretSimNode (Connect connectionConfig k) = do
  maybeSocket <- socketConnectImpl connectionConfig
  pure $ k maybeSocket
interpretSimNode (CloseConnection socket a) = do
  liftEffect $ socketCloseImpl socket
  pure a
interpretSimNode (SendSocketMessage socket message a) = do
  socketWriteMessageImpl socket $ message
  pure a
interpretSimNode (ReceiveSocketMessage socket k) = do
  maybeMessage <- socketReceiveImpl socket
  liftEffect $ Console.log $ "received socket message: " <> show maybeMessage
  pure $ k maybeMessage
interpretSimNode (ReceiveSystemMessage k) = do
  maybeMessage <- systemReceiveImpl
  liftEffect $ Console.log $ "received system message: " <> show maybeMessage
  pure $ k maybeMessage
interpretSimNode action = interpretPrinty action

-- add randomized readings with logic from python sim lib
-- interpretSimNode :: StateLangF a -> Aff a
-- - printy + actual sockets
-- interpretSimEspruino :: State -> Aff Unit
-- - simNode + wifi
-- from here, work in hello_ota dir to implement ota update functionality in purs

runPrinty :: Effect Unit
runPrinty = do
  Console.log "Hello printy printo!"
  _ <- runLaff $ runProgram interpretPrinty InitialState
  pure unit

runSimNode :: Effect Unit
runSimNode = do
  Console.log "Starting simulation"
  _ <- runLaff $ runProgram interpretSimNode InitialState
  pure unit

type DeviceActions =
  { now :: Laff Milliseconds
  , sleep :: Milliseconds -> Laff Unit
  , deepSleep :: Milliseconds -> Laff Unit
  , doAfter :: Milliseconds -> (Laff Unit) -> Laff Unit
  , readBatteryVoltage :: Laff (Maybe Number)
  , connect :: ConnectionConfig -> Laff (Maybe Socket)
  , closeConnection :: Socket -> Laff Unit
  , sendSocketMessage :: Socket -> String -> Laff Unit
  , receiveSocketMessage :: Socket -> Laff (Maybe SocketMessage)
  , receiveSystemMessage :: Laff (Maybe SystemMessage')
  , readSensor :: Laff (Maybe Number)
  , flashLights :: Laff Unit
  }

data SystemMessage'
  = SystemMessageSocketClosed'
  | SystemMessageAction' (Laff Unit)

instance Show SystemMessage' where
  show SystemMessageSocketClosed' = "SystemMessageSocketClosed"
  show (SystemMessageAction' _) = "SystemMessageAction"

foreign import systemReceiveImplJs2 :: (forall x. x -> Maybe x) -> (forall x. Maybe x) -> Laff (Maybe SystemMessage')

systemReceiveImpl' :: Laff (Maybe SystemMessage')
systemReceiveImpl' = systemReceiveImplJs2 Just Nothing

program' :: Config -> DeviceActions -> (State -> Laff State)
program' config actions =
  let
    handleSystemMessage :: ReceivingStateData -> SystemMessage' -> Laff State
    handleSystemMessage _ SystemMessageSocketClosed' = pure $ DeepSleepingState {socket: Nothing}
    handleSystemMessage stateData (SystemMessageAction' action) = do
      action
      pure $ ReceivingState stateData

    handleSocketMessage :: ReceivingStateData -> SocketMessage -> Laff State
    handleSocketMessage stateData (SocketMessageStoreFile name size) = do
      --actions.storeFile name size
      pure $ ReceivingState stateData
    handleSocketMessage _ SocketMessageReboot = do
      actions.deepSleep $ Milliseconds 0.0
      pure InitialState
    handleSocketMessage stateData SocketMessageReadSensor = do
      actions.readSensor >>= case _ of
        Nothing -> pure unit
        Just sensorReading -> actions.sendSocketMessage stateData.socket $ writeJSON {sensor_reading: sensorReading}
      pure $ ReceivingState stateData
    handleSocketMessage stateData SocketMessageReadBatteryVoltage = do
      actions.readBatteryVoltage >>= case _ of
        Nothing -> pure unit
        Just voltage -> actions.sendSocketMessage stateData.socket $ writeJSON {battery_voltage: voltage}
      pure $ ReceivingState stateData
    handleSocketMessage stateData SocketMessageFlashLights = do
      actions.flashLights
      pure $ ReceivingState stateData
    handleSocketMessage stateData SocketMessageDoThingA = pure $ ReceivingState stateData
    handleSocketMessage stateData SocketMessageDoThingB = pure $ ReceivingState stateData
  in
    case _ of
      InitialState ->
        actions.readBatteryVoltage >>= case _ of
          Just batteryVoltage | batteryVoltage < 3.3 ->
            pure $ DeepSleepingState {socket: Nothing}
          _ -> do
              actions.connect config.connectionConfig >>= case _ of
                Nothing -> pure $ DeepSleepingState {socket: Nothing}
                Just socket -> pure $ ReceivingState { timeLastSendBatteryVoltage: Milliseconds 0.0, socket: socket }
      ReceivingState stateData -> do
        fromMaybe (pure $ ReceivingPauseState stateData) =<< (runMaybeT $
          (MaybeT $ actions.receiveSystemMessage <#> map (handleSystemMessage stateData))
          <|>
          (MaybeT $ actions.receiveSocketMessage stateData.socket <#> map (handleSocketMessage stateData))
          <|>
          (MaybeT $ actions.readBatteryVoltage <#> map \batteryVoltage -> do
             nowMillis <- actions.now
             if nowMillis > stateData.timeLastSendBatteryVoltage <> config.batteryVoltageSendPeriod
               then do
                 liftEffect $ Console.log "Write battery voltage to socket"
                 actions.sendSocketMessage stateData.socket $ writeJSON {battery_voltage: batteryVoltage}
               else pure unit
             if batteryVoltage < 3.3
               then pure $ DeepSleepingState {socket: Just stateData.socket}
               else pure $ ReceivingPauseState $ stateData {timeLastSendBatteryVoltage = nowMillis}))
      ReceivingPauseState stateData -> do
        actions.sleep config.loopSleepPeriod
        pure $ ReceivingState stateData
      DeepSleepingState stateData -> do
        case stateData.socket of
          Nothing -> pure unit
          Just socket -> actions.closeConnection socket
        actions.deepSleep config.deepSleepPeriod
        pure InitialState

runProgram' :: DeviceActions -> State -> Laff Unit
runProgram' interpretedActions state = do
  state' <- program' { loopSleepPeriod: Milliseconds 3000.0
                     , batteryVoltageSendPeriod: Milliseconds 500.0
                     , deepSleepPeriod: Milliseconds 4000.0
                     , connectionConfig:
                       { timeout: Milliseconds 5000.0
                       --, host: "localhost"
                       , host: "192.168.182.1"
                       , port: 9222
                       , wifi_ssid: "hello"
                       , wifi_password: "hi"
                       }
                     }
                     interpretedActions
                     state
  runProgram' interpretedActions state'

interpretPrinty' :: DeviceActions
interpretPrinty' =
  { now: do
      nowMillis <- liftEffect $ EffectNow.now
      pure $ unInstant nowMillis
  , sleep: \millis -> do
      liftEffect $ Console.log ("sleeping for " <> show millis)
      liftEffect $ Console.log ""
      delay millis
  , deepSleep: \millis -> do
      liftEffect $ Console.log ("deep-sleeping for " <> show millis)
      liftEffect $ Console.log ""
      delay millis
  , doAfter: \millis action -> do
      liftEffect $ runLaff do
        delay millis
        action
  , readBatteryVoltage: do
      reading <- liftEffect do
        voltage <- random <#> \r -> r * 3.3 + 3.0
        choice <- randomInt 0 4
        pure if choice == 0 then Nothing else Just voltage
      liftEffect $ Console.log $ "read battery voltage: " <> show reading
      pure reading
  , connect: \opts -> do
      socket <- liftEffect $ randomInt 0 4 <#> \r -> if r == 0 then Nothing else Just dummySocket
      liftEffect $ Console.log $ "connecting created: " <> show (isJust socket)
      pure socket
  , closeConnection: \socket -> do
      liftEffect $ Console.log "close connection"
  , sendSocketMessage: \socket message -> do
      liftEffect $ Console.log $ "send message " <> message
  , receiveSocketMessage: \socket -> do
      message <- liftEffect $ randomInt 0 (7 - 1 + 10) <#> case _ of
        0 -> Just $ SocketMessageStoreFile ".boot0" 55
        1 -> Just SocketMessageReboot
        2 -> Just SocketMessageReadSensor
        3 -> Just SocketMessageReadBatteryVoltage
        4 -> Just SocketMessageFlashLights
        5 -> Just SocketMessageDoThingA
        6 -> Just SocketMessageDoThingB
        _ -> Nothing
      liftEffect $ Console.log $ "received socket message: " <> show message
      pure message
  , receiveSystemMessage: do
      message <- liftEffect $ randomInt 0 5 <#> case _ of
        0 -> Just SystemMessageSocketClosed'
        1 -> Just $ SystemMessageAction' (liftEffect $ Console.log "doAfter action hello")
        _ -> Nothing
      liftEffect $ Console.log $ "received system message: " <> show message
      pure message
  , readSensor: do
      reading <- liftEffect do
        value <- random <#> \r -> r * 5.0
        choice <- randomInt 0 (7 - 1 + 10)
        pure if choice == 0 then Nothing else Just value
      liftEffect $ Console.log $ "sensor reading " <> show reading
      pure reading
  , flashLights: do
      liftEffect $ Console.log "Flashing lights"
  }

runPrinty2 :: Effect Unit
runPrinty2 = do
  Console.log "Hello printy printo!"
  _ <- runLaff $ runProgram' interpretPrinty' InitialState
  pure unit

foreign import socketConnectImplJs2 ::
  (forall x. x -> Maybe x) -> (forall x. Maybe x) -> SystemMessage' -> ConnectionConfig -> Laff (Maybe Socket)

socketConnectImpl' :: ConnectionConfig -> Laff (Maybe Socket)
socketConnectImpl' = socketConnectImplJs2 Just Nothing SystemMessageSocketClosed'

interpretSim :: DeviceActions
interpretSim =
  R.merge
  { deepSleep: \millis -> do
      liftEffect $ Console.log ("deep-sleeping for " <> show millis)
      liftEffect $ Console.log ""
      deepSleepImpl millis
  , connect: \connectionConfig -> do
      maybeSocket <- socketConnectImpl' connectionConfig
      pure maybeSocket
  , closeConnection: \socket -> do
      liftEffect $ socketCloseImpl socket
  , sendSocketMessage: \socket message -> do
      socketWriteMessageImpl socket message
  , receiveSocketMessage: \socket -> do
      maybeMessage <- socketReceiveImpl socket
      liftEffect $ Console.log $ "received socket message: " <> show maybeMessage
      pure maybeMessage
  , receiveSystemMessage: do
      maybeMessage <- systemReceiveImpl'
      liftEffect $ Console.log $ "received system message: " <> show maybeMessage
      pure maybeMessage
  }
  interpretPrinty'

runSim :: Effect Unit
runSim = do
  Console.log "Hello Simmo!"
  _ <- runLaff $ runProgram' interpretSim InitialState
  pure unit

foreign import flashLightsImpl :: Laff Unit

interpretMCU :: DeviceActions
interpretMCU =
  R.merge
  { flashLights: do
      liftEffect $ Console.log "FLASH FOR REAL"
      flashLightsImpl
  }
  interpretSim

runMCU :: Effect Unit
runMCU = do
  Console.log "Hello MCU!"
  _ <- runLaff $ runProgram' interpretMCU InitialState
  pure unit
