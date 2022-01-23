{-
    Take reactive action to keep the farm operating well
    Including:
    - If the tank is full, make sure the bore pump isn't running
-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (pack)
import Network.MQTT.Client (MessageCallback(..), MQTTClient, MQTTConfig(..), Topic, Property,
                            connectURI, mqttConfig, publish, subOptions, subscribe, waitForClient)
import Network.URI (parseURI)

import Farm.Config (mqttServer)

_publishMessage :: String -> String -> Bool -> IO ()
_publishMessage topic message retain = do
  let (Just uri) = parseURI mqttServer
  mc <- connectURI mqttConfig uri
  putStrLn $ topic ++ " " ++ message
  publish mc (pack topic) (BL.pack message) retain

publishMessage topic message = _publishMessage topic message False

publishRetainedMessage topic message = _publishMessage topic message True

mqttCallback :: MQTTClient -> Topic -> BL.ByteString -> [Property] -> IO ()
mqttCallback mqttclient topic message properties =
  case topic of
    "tank_monitor/tank_level/out/high_level_switch" ->
      if message == "WET"
      then publishMessage "pump/BorePump/stop" "{}"
      else return ()
    "farm_monitor/in/pump/BorePump/start" ->
      publishMessage "pump/BorePump/start" $ BL.unpack message
    _ -> return ()

main = do
  let (Just uri) = parseURI mqttServer
  mc <- connectURI mqttConfig{_msgCB=SimpleCallback mqttCallback} uri
  subscribe mc [("#", subOptions)] []
  waitForClient mc   -- wait for the the client to disconnect
