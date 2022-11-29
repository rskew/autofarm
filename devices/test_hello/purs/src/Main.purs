module Main where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..), isJust)
import Data.Time.Duration (Milliseconds(..))
import Data.DateTime.Instant (unInstant)
import Data.Number.Format (toString)
import Control.Monad.Free (Free, liftF, runFreeM)
import Effect (Effect)
import Effect.Now as EffectNow
import Effect.Laff (Laff, delay, runLaff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Random (random, randomInt)
import Simple.JSON (writeJSON)

data State
  = InitialState
  | ReceivingState
    { socket :: Socket
    , timeLastSendBatteryVoltage :: Milliseconds
    }
  | DeepSleepingState { socket :: Maybe Socket }

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

data Message
  = MessageStoreFile String Int
  | MessageReboot
  | MessageReadSensor
  | MessageReadBatteryVoltage
  | MessageFlashLights
  | MessageDoThingA
  | MessageDoThingB

derive instance genericMessage :: Generic Message _
instance showMessage :: Show Message where
  show = genericShow

data StateLangF a
  -- = ToJson _ (String -> a)
  = Now (Milliseconds -> a)
  | Sleep Milliseconds a
  | DeepSleep Milliseconds a
  | ReadBatteryVoltage (Maybe Number -> a)
  | Connect ConnectionConfig (Maybe Socket -> a)
  | Close Socket a
  | SendMessage Socket String a
  | ReceiveMessage Socket (Maybe Message -> a)
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
  map f (ReadBatteryVoltage k) = ReadBatteryVoltage (f <<< k)
  map f (Connect config k) = Connect config (f <<< k)
  map f (Close socket a) = Close socket (f a)
  map f (SendMessage socket message a) = SendMessage socket message (f a)
  map f (ReceiveMessage socket k) = ReceiveMessage socket (f <<< k)
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

readBatteryVoltage :: StateLang (Maybe Number)
readBatteryVoltage = liftF $ ReadBatteryVoltage identity

connect :: ConnectionConfig -> StateLang (Maybe Socket)
connect connectionConfig = liftF $ Connect connectionConfig identity

close :: Socket -> StateLang Unit
close socket = liftF $ Close socket unit

sendMessage :: Socket -> String -> StateLang Unit
sendMessage socket message = liftF $ SendMessage socket message unit

receiveMessage :: Socket -> StateLang (Maybe Message)
receiveMessage socket = liftF $ ReceiveMessage socket identity

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
program config
 = case _ of
     InitialState ->
       readBatteryVoltage >>= case _ of
         Nothing -> pure $ DeepSleepingState {socket: Nothing}
         Just batteryVoltage -> do
           if batteryVoltage < 3.3
           then pure $ DeepSleepingState {socket: Nothing}
           else
             connect config.connectionConfig >>= case _ of
               Nothing -> pure $ DeepSleepingState {socket: Nothing}
               Just socket -> do
                 sendMessage socket $ writeJSON {batteryVoltage: batteryVoltage}
                 nowMillis <- now
                 pure $ ReceivingState { timeLastSendBatteryVoltage: nowMillis, socket : socket }
     ReceivingState state -> do
       sleep config.loopSleepPeriod
       receiveMessage state.socket >>= case _ of
         Just (MessageStoreFile name size) -> do
           storeFile name size
           pure $ ReceivingState state
         Just MessageReboot -> do
           deepSleep $ Milliseconds 0.0
           pure InitialState
         Just MessageReadSensor -> do
           readSensor >>= case _ of
             Nothing -> pure unit
             Just sensorReading -> sendMessage state.socket $ writeJSON {sensorReading: sensorReading}
           pure $ ReceivingState state
         Just MessageReadBatteryVoltage -> do
           readBatteryVoltage >>= case _ of
             Nothing -> pure unit
             Just voltage -> sendMessage state.socket $ writeJSON {batteryVoltage: voltage}
           pure $ ReceivingState state
         Just MessageFlashLights -> do
           flashLights
           pure $ ReceivingState state
         Just MessageDoThingA -> do
           doThingA
           pure $ ReceivingState state
         Just MessageDoThingB -> do
           doThingB
           pure $ ReceivingState state
         Nothing ->
           readBatteryVoltage >>= case _ of
             Nothing -> pure $ ReceivingState state
             Just batteryVoltage -> do
               nowMillis <- now
               if nowMillis > state.timeLastSendBatteryVoltage <> config.batteryVoltageSendPeriod
                 then sendMessage state.socket $ writeJSON {batteryVoltage: batteryVoltage}
                 else pure unit
               if batteryVoltage < 3.3
                 then pure $ DeepSleepingState {socket: Nothing}
                 else pure $ ReceivingState $ state {timeLastSendBatteryVoltage = nowMillis}
     DeepSleepingState state -> do
       case state.socket of
         Nothing -> pure unit
         Just socket -> close socket
       deepSleep config.deepSleepPeriod
       pure InitialState

runProgram :: (forall a. StateLangF a -> Laff a) -> State -> Laff Unit
runProgram interpreter s =
  let programLoopStep = program { loopSleepPeriod: Milliseconds 3000.0
                                , batteryVoltageSendPeriod: Milliseconds 500.0
                                , deepSleepPeriod: Milliseconds 4000.0
                                , connectionConfig:
                                  { timeout: Milliseconds 5000.0
                                  , host: "localhost"
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
interpretPrinty (ReadBatteryVoltage k) = do
  reading <- liftEffect do
    voltage <- random <#> \r -> r * 3.3 + 3.0
    choice <- randomInt 0 4
    pure if choice == 0 then Nothing else Just voltage
  liftEffect $ Console.log $ "read battery voltage: " <> show reading
  pure (k reading)
interpretPrinty (Connect _ k) = do
  socket <- liftEffect $ randomInt 0 4 <#> \r -> if r == 0 then Nothing else Just dummySocket
  liftEffect $ Console.log $ "connecting created: " <> show (isJust socket)
  pure (k socket)
interpretPrinty (Close _ a) = do
  liftEffect $ Console.log "close connection"
  pure a
interpretPrinty (SendMessage _ message a) = do
  liftEffect $ Console.log $ "send message " <> message
  pure a
interpretPrinty (ReceiveMessage _ k) = do
  message <- liftEffect $ randomInt 0 (7 - 1 + 10) <#> case _ of
    0 -> Just $ MessageStoreFile ".boot0" 55
    1 -> Just MessageReboot
    2 -> Just MessageReadSensor
    3 -> Just MessageReadBatteryVoltage
    4 -> Just MessageFlashLights
    5 -> Just MessageDoThingA
    6 -> Just MessageDoThingB
    _ -> Nothing
  liftEffect $ Console.log $ "received message: " <> show message
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

foreign import socketConnectImpl ::
  (forall x. x -> Maybe x) -> (forall x. Maybe x) -> ConnectionConfig -> Laff (Maybe Socket)

socketConnect :: ConnectionConfig -> Laff (Maybe Socket)
socketConnect = socketConnectImpl Just Nothing

foreign import socketClose :: Socket -> Effect Unit
foreign import socketWrite :: Socket -> String -> Laff Unit
foreign import socketReceiveImpl ::
  (forall x. x -> Maybe x) ->
  (forall x. Maybe x) ->
  (String -> Int -> Message) ->
  Message ->
  Message ->
  Message ->
  Message ->
  Message ->
  Message ->
  Socket -> Laff (Maybe Message)

socketReceive :: Socket -> Laff (Maybe Message)
socketReceive = socketReceiveImpl
  Just Nothing
  MessageStoreFile
  MessageReboot
  MessageReadSensor
  MessageReadBatteryVoltage
  MessageFlashLights
  MessageDoThingA
  MessageDoThingB

interpretSimNode :: forall a. StateLangF a -> Laff a
interpretSimNode (Connect connectionConfig k) = do
  maybeSocket <- socketConnect connectionConfig
  pure $ k maybeSocket
interpretSimNode (Close socket a) = do
  liftEffect $ socketClose socket
  pure a
interpretSimNode (SendMessage socket message a) = do
  socketWrite socket message
  pure a
interpretSimNode (ReceiveMessage socket k) = do
  maybeMessage <- socketReceive socket
  liftEffect $ Console.log $ "received message: " <> show maybeMessage
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
