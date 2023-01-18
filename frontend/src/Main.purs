module Main where

import Prelude

import Affjax (defaultRequest, printError)
import Affjax.ResponseFormat (json)
import Affjax.Web (request)
import Control.Coroutine as CR
import Control.Coroutine.Aff (emit)
import Control.Coroutine.Aff as CRA
import Control.Monad.Except (runExcept)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Foreign (F, Foreign, unsafeToForeign, readString)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Web.Event.EventTarget as EET
import Web.HTML as WH
import Web.HTML.Location as WL
import Web.HTML.Window as WW
import Web.Socket.Event.EventTypes as WSET
import Web.Socket.Event.MessageEvent as ME
import Web.Socket.WebSocket as WS

import Data.Argonaut.Decode (decodeJson, printJsonDecodeError)

import MainComponent as MainComponent
import MainComponent (mainComponent)

-- A producer coroutine that emits messages that arrive from the websocket.
wsProducer :: WS.WebSocket -> CR.Producer String Aff Unit
wsProducer socket = CRA.produce \emitter -> do
  listener <- EET.eventListener \ev -> do
    for_ (ME.fromEvent ev) \msgEvent ->
      for_ (readHelper readString (ME.data_ msgEvent)) \msg ->
        emit emitter msg
  EET.addEventListener
    WSET.onMessage
    listener
    false
    (WS.toEventTarget socket)
  where
  readHelper :: forall a b. (Foreign -> F a) -> b -> Maybe a
  readHelper read =
    either (const Nothing) Just <<< runExcept <<< read <<< unsafeToForeign

-- A consumer coroutine that takes the `query` function from our component IO
-- record and sends `ReceiveMessage` queries in when it receives inputs from the
-- producer.
wsConsumer :: (forall a. MainComponent.Query a -> Aff (Maybe a)) -> CR.Consumer String Aff Unit
wsConsumer query = CR.consumer \msg -> do
  void $ query $ H.mkTell $ MainComponent.ReceiveMessage msg
  pure Nothing

-- A handler for messages from our component IO that sends them to the server
-- using the websocket
wsSender :: WS.WebSocket -> MainComponent.Message -> Effect Unit
wsSender socket = case _ of
  MainComponent.OutputMessage msgContents ->
    WS.sendString socket msgContents

type ResponseBody = {wsPath :: String, sessionToken :: String}

main :: Effect Unit
main = do
  HA.runHalogenAff do
    eitherResponse <- H.liftAff $ request $ defaultRequest
                                          { responseFormat = json
                                          , method = Left GET
                                          , url = "/authorize"
                                          }
    case lmap printError eitherResponse
         >>= (_.body >>> decodeJson >>> lmap printJsonDecodeError)
         :: Either String ResponseBody
    of
      Left error ->
        H.liftEffect $ Console.log $ "Failed to authorize: " <> error
      Right authResponse -> do
        body <- HA.awaitBody
        io <- runUI mainComponent unit body
        wsUrl <- liftEffect $ getWSUrl authResponse.wsPath
        connection <- H.liftEffect $ WS.create wsUrl []
        H.liftEffect do
          listener <- EET.eventListener \_ ->
            WS.sendString connection $ "Authorization: Bearer " <> authResponse.sessionToken
          EET.addEventListener
            WSET.onOpen
            listener
            false
            (WS.toEventTarget connection)

        -- Subscribe to all output messages from our component
        _ <- H.liftEffect $ HS.subscribe io.messages $ wsSender connection

        -- Connecting the consumer to the producer initializes both,
        -- feeding queries back to our component as messages are received.
        H.liftAff $ CR.runProcess (wsProducer connection CR.$$ wsConsumer io.query)

getWSUrl :: String -> Effect (String)
getWSUrl path = do
  window <- WH.window
  location <- WW.location window
  host <- WL.host location
  protocol <- WL.protocol location
  pure $ (if protocol == "https:" then "wss://" else "ws://") <> host <> path
