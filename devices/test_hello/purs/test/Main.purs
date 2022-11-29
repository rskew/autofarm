module Test.Main where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Console as Console
import Effect.Class (liftEffect)

import Effect.Laff (Laff, delay, httpRequest, runLaff)

someLaff :: String -> Laff String
someLaff url = do
  liftEffect $ Console.log $ "Fetching " <> url
  ip <- httpRequest url
  delay $ Milliseconds 500.0
  liftEffect $ Console.log "hello from lifted Effect"
  delay $ Milliseconds 500.0
  liftEffect $ Console.log ip
  pure ip

main :: Effect Unit
main = do
  runLaff $ someLaff "ifconfig.me"
  runLaff $ someLaff "api.ipify.org"
  runLaff $ someLaff "icanhazip.com"
