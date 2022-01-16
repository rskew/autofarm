{-
    Log mqtt messages to a file, prepended by the timestamp received.
-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ByteString.Lazy.UTF8 (toString)
import Data.List (intercalate)
import Data.Text (unpack)
import Data.Time
import Network.MQTT.Client
import Network.URI
import Options.Generic

import Farm.Config

data Options = Options {logFile :: String}
  deriving (Generic, Show, Read)
instance ParseRecord Options

mqttCallback :: String -> MQTTClient -> Topic -> BL.ByteString -> [Property] -> IO ()
mqttCallback logFile _ topic message properties = do
  now <- getZonedTime
  -- Format time including milliseconds
  let nowStr = take 23 $ formatTime defaultTimeLocale "%F %T%Q" now
  appendFile logFile $ intercalate " " [nowStr, unpack topic, toString message, "\n"]

main = do
  Options logFile <- (getRecord "Farm logger") :: IO Options
  let (Just uri) = parseURI mqttServer
  mc <- connectURI mqttConfig{_msgCB=SimpleCallback $ mqttCallback logFile} uri
  subscribe mc [("#", subOptions)] []
  waitForClient mc   -- wait for the the client to disconnect
