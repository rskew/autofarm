{-
    Take reactive action to keep the farm operating well
    Including:
    - If the tank is full, make sure the bore pump isn't running
-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
module Main where

import Control.Concurrent (MVar, newMVar, readMVar, swapMVar, threadDelay)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (pack, unpack)
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

mqttCallback :: MVar Bool -> MQTTClient -> Topic -> BL.ByteString -> [Property] -> IO ()
mqttCallback tankFullMVar mqttclient topic message properties = do
  case (unpack topic, BL.unpack message) of
    ("tank_monitor/out/tank_level", "FULL") -> do
      putStrLn "Tank is full, stopping bore pump"
      publishMessage "pump/BorePump/stop" "{}"
      swapMVar tankFullMVar True
      return ()
    ("tank_monitor/out/tank_level", _) -> do
      swapMVar tankFullMVar False
      return ()
    ("farm_monitor/in/pump/BorePump/start", _) -> do
      tankFull <- readMVar tankFullMVar
      if tankFull
      then do
        putStrLn "Tank is full, not starting bore pump"
        return ()
      else do
        putStrLn "Tank not full, starting bore pump"
        publishMessage "pump/BorePump/start" $ BL.unpack message
        return ()
    _ -> return ()

main = do
  let (Just uri) = parseURI mqttServer
  tankFullMVar <- newMVar True
  mc <- connectURI mqttConfig{_msgCB=SimpleCallback $ mqttCallback tankFullMVar} uri
  subscribe mc [("#", subOptions)] []
  waitForClient mc   -- wait for the the client to disconnect
