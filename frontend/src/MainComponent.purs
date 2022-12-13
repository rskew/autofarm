module MainComponent where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), lift, runMaybeT)
import Data.Maybe (Maybe(..))
import Data.MediaType (MediaType(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Console as Console
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Web.DOM.Element as WDE
import Web.DOM.HTMLCollection as HTMLCollection
import Web.DOM.ParentNode as WDPN
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML as WH
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLDocument.ReadyState as ReadyState
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as WW
import Web.HTML.Event.EventTypes as WET
import Web.UIEvent.MouseEvent.EventTypes as MET

type Slot = H.Slot Query Message

data Query a = ReceiveMessage String a

data Message = OutputMessage String

type State
  = { ws_authenticated :: Boolean
    }

data Action
  = Initialize
  | DocumentLoaded
  | FarmSVGDocumentLoaded
  | TopRowMouseOver
  | TopRowMouseLeave
  | SendMessage String

mainComponent :: forall i. H.Component Query i Message Aff
mainComponent =
  H.mkComponent
    { initialState: \_ -> { ws_authenticated : false
                          }
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
    [ HP.id "farm-svg"
    , HP.prop (HH.PropName "data") "farm.svg"
    , HP.type_ (MediaType "image/svg+xml")
    ]
    []

handleAction :: forall cs m. MonadAff m =>
                Action → H.HalogenM State Action cs Message m Unit
handleAction = case _ of
  Initialize -> do
    H.liftEffect $ Console.log "Initialize"

    -- TODO test FarmSVGLoaded runs if the load event fires after Initialize
    document <- H.liftEffect (WH.window >>= WW.document)
    H.liftEffect (HTMLDocument.readyState document) >>= case _ of
      ReadyState.Complete -> handleAction DocumentLoaded
      _ -> H.liftEffect (HTMLDocument.body document) >>= case _ of
        Nothing -> pure unit
        Just body ->
          void $ H.subscribe $
            eventListener
              WET.load
              (HTMLElement.toEventTarget body)
              (const $ Just DocumentLoaded)

  DocumentLoaded -> void $ runMaybeT do
    lift $ H.liftEffect $ Console.log "DocumentLoaded"
    document <- lift $ H.liftEffect $ WW.document =<< WH.window
    farmSVGElement <- MaybeT $ H.liftEffect $ getElementById "farm-svg" $ HTMLDocument.toNonElementParentNode document
    void $ lift $ H.subscribe $
      eventListener
        WET.load
        (WDE.toEventTarget farmSVGElement)
        (const $ Just FarmSVGDocumentLoaded)

  FarmSVGDocumentLoaded -> void $ runMaybeT do
    lift $ H.liftEffect $ Console.log "FarmSVGDocumentLoaded"
    topRowElement <- MaybeT $ H.liftEffect $ getSVGElementById "farm-svg" "top_row_drippers"
    void $ lift $ H.subscribe $
      eventListener
        MET.mouseover
        (WDE.toEventTarget topRowElement)
        (const $ Just TopRowMouseOver)
    void $ lift $ H.subscribe $
      eventListener
        MET.mouseleave
        (WDE.toEventTarget topRowElement)
        (const $ Just TopRowMouseLeave)
    lift $ H.liftEffect $ Console.log "Event handler added for top_row_drippers mousedown"

  TopRowMouseOver -> void $ H.liftEffect $ runMaybeT do
    lift $ Console.log "TopRowMouseOver"
    topRowElement <- MaybeT $ getSVGElementById "farm-svg" "top_row_drippers"
    topRowRectElement <- MaybeT $ HTMLCollection.item 0 =<< WDPN.children (WDE.toParentNode topRowElement)
    lift $ WDE.setAttribute "style" "fill-opacity: 1; fill: blue" topRowRectElement

  TopRowMouseLeave -> void $ H.liftEffect $ runMaybeT do
    lift $ Console.log "TopRowMouseLeave"
    topRowElement <- MaybeT $ getSVGElementById "farm-svg" "top_row_drippers"
    topRowRectElement <- MaybeT $ HTMLCollection.item 0 =<< WDPN.children (WDE.toParentNode topRowElement)
    lift $ WDE.setAttribute "style" "fill-opacity: 0; stroke: black; stroke-width: 0.2" topRowRectElement

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
    else do
        pure unit
    pure (Just a)

foreign import _getSVGElementById :: Maybe WDE.Element -> (WDE.Element -> Maybe WDE.Element) -> String -> String -> Effect (Maybe WDE.Element)

getSVGElementById :: String -> String -> Effect (Maybe WDE.Element)
getSVGElementById = _getSVGElementById Nothing Just
