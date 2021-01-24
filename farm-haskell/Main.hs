{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Aeson as Aeson
import Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Text hiding (head)
import Data.Time
import Data.Time.Clock.POSIX
import Network.MQTT.Client
import Network.URI
import Options.Generic
import Text.Read
import System.IO

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
  Irrigate zone -> "irrigate/" ++ show zone
  Pump pump -> "pump/" ++ show pump

data FarmNode
  = IrrigationController
  | BorePumpController
  deriving (Generic, Show, Read)
instance ParseRecord FarmNode
instance ParseFields FarmNode
instance ParseField FarmNode

farmNodeTopic :: FarmNode -> String
farmNodeTopic = \case
  IrrigationController -> "irrigation_controller"
  BorePumpController -> "bore_pump_controller"

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

publishMessage :: String -> String -> IO ()
publishMessage topic message = do
  let (Just uri) = parseURI mqttServer
  mc <- connectURI mqttConfig uri
  putStrLn $ topic ++ " " ++ message
  publish mc (pack topic) (BL.pack message) False

main :: IO ()
main = do
  x <- (getRecord "Farm control") :: IO Action
  now <- round <$> getPOSIXTime
  case x of
    Start farmVerb duration -> do
      let
        topic = (farmVerbTopic farmVerb) ++ "/start"
        msg :: Map Text Aeson.Value
        msg = Map.fromList [("timestamp", jsonInt now), ("duration", jsonInt duration)]
      publishMessage topic $ (toString . encode) msg
    Stop farmVerb -> do
      let
        topic = (farmVerbTopic farmVerb) ++ "/stop"
        msg :: Map Text Aeson.Value
        msg = Map.fromList [("timestamp", jsonInt now)]
      publishMessage topic $ (toString . encode) msg
    Ping farmNode -> do
      let
        topic = (show farmNode) ++ "/ping"
        msg :: Map Text Aeson.Value
        msg = Map.fromList [("timestamp", jsonInt now)]
      publishMessage topic $ (toString . encode) msg
    Reboot farmNode -> do
      let
        topic = (farmNodeTopic farmNode) ++ "/reboot"
        msg :: Map Text Aeson.Value
        msg = Map.fromList [("timestamp", jsonInt now)]
      publishMessage topic $ (toString . encode) msg
    UpdateFile farmNode filename -> do
      let
        topic = (farmNodeTopic farmNode) ++ "/update_file/" ++ filename
      -- TODO don't asume user is running from farm-haskell folder
      handle <- openFile ("../irrigation_controller/" ++ filename) ReadMode
      msg <- hGetContents handle
      publishMessage topic msg
