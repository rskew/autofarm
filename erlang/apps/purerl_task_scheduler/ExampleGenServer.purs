module ExampleGenServer where

import Prelude
import Erl.Atom (atom)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Pinto (RegistryName(..), StartLinkResult)
import Pinto.GenServer (InitResult(..), ServerPid, ServerType)
import Pinto.GenServer as GenServer
import Pinto.Types (RegistryReference(..))

type ExampleGenServerStartArgs
  = {}

type State
  = {}

data Msg
  = Hello

serverName :: RegistryName (ServerType Unit Unit Msg State)
serverName = Local $ atom "example_gen_server"

startLink :: ExampleGenServerStartArgs -> Effect (StartLinkResult (ServerPid Unit Unit Msg State))
startLink args = GenServer.startLink $ (GenServer.defaultSpec $ init args) { name = Just serverName, handleInfo = Just handleInfo }

init :: ExampleGenServerStartArgs -> GenServer.InitFn Unit Unit Msg State
init _args = do
  pure $ InitOk {}

handleInfo :: GenServer.InfoFn Unit Unit Msg State
handleInfo msg state =
  case msg of
    Hello -> do
      liftEffect $ Console.log "hello info"
      pure $ GenServer.return state

hello :: Effect Unit
hello = GenServer.cast (ByName serverName) (\s -> do
                                               liftEffect $ Console.log "hello cast"
                                               pure $ GenServer.return s)
