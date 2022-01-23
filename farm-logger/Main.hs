{-
    Log mqtt messages to a file, prepended by the timestamp received.
-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Exception (IOException, catch)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ByteString.Lazy.UTF8 (toString)
import Data.List (intercalate)
import Data.Text (unpack)
import Data.Time
import Network.MQTT.Client
import Network.URI
import Options.Generic
import System.IO

import Farm.Config

data Options = Options {logFile :: String}
  deriving (Generic, Show, Read)
instance ParseRecord Options

mqttCallback :: String -> MQTTClient -> Topic -> BL.ByteString -> [Property] -> IO ()
mqttCallback logFile mqttclient topic message properties = do
  now <- getZonedTime
  -- Format time including milliseconds
  let nowStr = take 23 $ formatTime defaultTimeLocale "%F %T%Q" now
  catch (appendFile logFile $ intercalate " " [nowStr, unpack topic, toString message, "\n"])
      (\e -> do let err = show (e :: IOException)
                hPutStr stderr ("Warning: Couldn't open " ++ logFile ++ ": " ++ err)
                _ <- mqttCallback logFile mqttclient topic message properties
                return ())

main = do
  Options logFile <- (getRecord "Farm logger") :: IO Options
  let (Just uri) = parseURI mqttServer
  mc <- connectURI mqttConfig{_msgCB=SimpleCallback $ mqttCallback logFile} uri
  subscribe mc [("#", subOptions)] []
  waitForClient mc   -- wait for the the client to disconnect
