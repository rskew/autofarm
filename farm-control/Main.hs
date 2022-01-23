{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
module Main where

import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Data.Aeson as Aeson
import Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Text hiding (head)
import Data.Time
import Data.Time.Clock.POSIX
import Network.MQTT.Client (MessageCallback(..), MQTTClient, MQTTConfig(..), Topic, Property,
                            connectURI, disconnect, mqttConfig, publish, subOptions, subscribe, unsubscribe)
import Network.MQTT.Types (DiscoReason(..))
import Network.URI
import Options.Generic
import Text.Read
import System.IO
import System.Timeout (timeout)

import Farm.Config

data IrrigationZone
  = TopRow
  | Pumpkins
  | PolyTunnel
  | All
  deriving (Generic, Show, Read)
instance ParseRecord IrrigationZone
instance ParseFields IrrigationZone
instance ParseField IrrigationZone

data PumpName
  = BorePump
  | RainwaterTankPump
  deriving (Generic, Show, Read)
instance ParseRecord PumpName
instance ParseFields PumpName
instance ParseField PumpName

data FarmVerb
  = Irrigate IrrigationZone
  | Pump PumpName
  deriving (Generic, Show, Read)
instance ParseRecord FarmVerb
instance ParseFields FarmVerb
instance ParseField FarmVerb

farmVerbTopic :: FarmVerb -> String
farmVerbTopic = \case
  Irrigate zone -> "irrigate/in/" ++ show zone
  Pump pump -> "pump/in/" ++ show pump

data FarmNode
  = IrrigationController
  | BorePumpController
  | TankMonitor
  deriving (Generic, Show, Read)
instance ParseRecord FarmNode
instance ParseFields FarmNode
instance ParseField FarmNode

farmNodeTopic :: FarmNode -> String
farmNodeTopic = \case
  IrrigationController -> "irrigation_controller"
  BorePumpController -> "bore_pump_controller"
  TankMonitor -> "tank_monitor"

data Action
  = Start {farmVerb :: FarmVerb, duration :: Integer}
  | Stop {farmVerb :: FarmVerb}
  | Ping {farmNode :: FarmNode}
  | Reboot {farmNode :: FarmNode}
  | UpdateFile {farmNode :: FarmNode, filename :: String}
  deriving (Generic, Show, Read)
instance ParseRecord Action
instance ParseFields Action
instance ParseField Action

jsonInt :: Integer -> Aeson.Value
jsonInt = Aeson.Number . fromInteger

publishMessage :: String -> String -> Bool -> IO ()
publishMessage topic message retain = do
  let (Just uri) = parseURI mqttServer
  mc <- connectURI mqttConfig uri
  putStrLn $ topic ++ " " ++ message
  publish mc (pack topic) (BL.pack message) retain

main :: IO ()
main = do
  x <- (getRecord "Farm control") :: IO Action
  now <- round <$> getPOSIXTime
  case x of
    Start farmVerb duration -> do
      let
        topic = (farmVerbTopic farmVerb) ++ "/in/start"
        msg :: Map Text Aeson.Value
        msg = Map.fromList [("timestamp", jsonInt now), ("duration", jsonInt duration)]
      case farmVerb of
        -- Delegate starting the BorePump to the farm-monitor.
        -- The farm-monitor will switch off the pump when the tank is filled.
        Pump BorePump -> do
          let
            (Just uri) = parseURI mqttServer
            redirectedTopic = "farm_monitor/in/pump/BorePump/start"
          publishMessage redirectedTopic ((toString . encode) msg) False
        _ -> publishMessage topic ((toString . encode) msg) False
    Stop farmVerb -> do
      let
        topic = (farmVerbTopic farmVerb) ++ "/in/stop"
        msg :: Map Text Aeson.Value
        msg = Map.fromList [("timestamp", jsonInt now)]
      publishMessage topic ((toString . encode) msg) False
    Ping farmNode -> do
      let
        topic = (farmNodeTopic farmNode) ++ "/in/ping"
        msg :: Map Text Aeson.Value
        msg = Map.fromList [("timestamp", jsonInt now)]
      publishMessage topic ((toString . encode) msg) False
    Reboot farmNode -> do
      let
        topic = (farmNodeTopic farmNode) ++ "/in/reboot"
        msg :: Map Text Aeson.Value
        msg = Map.fromList [("timestamp", jsonInt now)]
      publishMessage topic ((toString . encode) msg) True
    UpdateFile farmNode filename -> do
      let
        topic = (farmNodeTopic farmNode) ++ "/in/update_file/" ++ filename
      handle <- openFile filename ReadMode
      msg <- hGetContents handle
      publishMessage topic msg True
