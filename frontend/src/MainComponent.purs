module MainComponent where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Console as Console
import Halogen as H
import Halogen.HTML as HH

type Slot = H.Slot Query Message

data Query a = ReceiveMessage String a

data Message = OutputMessage String

type State
  = { ws_authenticated :: Boolean }

data Action
  = Initialize
  | SendMessage String

mainComponent :: forall i. H.Component Query i Message Aff
mainComponent =
  H.mkComponent
    { initialState: \_ -> { ws_authenticated : false }
    , render
    , eval: H.mkEval
              H.defaultEval
                { handleAction = handleAction
                , handleQuery = handleQuery
                , initialize = Just Initialize
                }
    }

render :: State -> H.ComponentHTML Action () Aff
render _ =
  HH.object
    [ HP.data "farm.svg"
    , HP.type "image/svg+xml"
    , HP.id "farm_svg"
    , HP.width "100%"
    , HP.height "100%"
    ]
    []
    -- <object data="alpha.svg" type="image/svg+xml"
    --  id="alphasvg" width="100%" height="100%"></object>

handleAction :: forall cs m. MonadAff m =>
                Action → H.HalogenM State Action cs Message m Unit
handleAction = case _ of
  Initialize -> do
    H.liftEffect $ Console.log "Initialize"

  SendMessage msg ->
    H.raise $ OutputMessage msg

handleQuery :: forall cs m a. MonadAff m =>
               Query a -> H.HalogenM State Action cs Message m (Maybe a)
handleQuery = case _ of
  ReceiveMessage msg a -> do
    H.liftEffect $ Console.log $ "Received: " <> msg
    state <- H.get
    if msg == "Authorization Successful" && not state.ws_authenticated
    then do
        H.modify_ _{ws_authenticated = true}
        handleAction $ SendMessage "hello!"
        handleAction $ SendMessage "howdy!"
        handleAction $ SendMessage "hi!"
    else
        pure unit
    pure (Just a)
