module EventScheduler where

import Prelude
import Erl.Atom (atom)
import Data.Maybe (Maybe(..))
import Data.Time (Time(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Pinto (RegistryName(..), StartLinkResult)
import Pinto.GenServer (InitResult(..), ServerPid, ServerType)
import Pinto.GenServer as GenServer
import Pinto.Types (RegistryReference(..))

-- ===================================================================
-- Types
-- ===================================================================

type EventSchedulerStartArgs
  = {}

type State
  = {}

data Msg
  = Hello

data EventError
  = EventNotActionedError
  | EventPartiallyActionedError

type EventId = String

type EventSchedule
  = { name :: String
    , action :: EventId -> Effect Unit
    , runAfter :: Time
    , runBefore :: Time
    , timeoutSeconds :: Int
    , errorHandler :: EventError -> Effect Unit
    , everyDays :: Int
    }

data EventState
  = EventComplete
  | EventNotComplete String

-- ===================================================================
-- API
-- ===================================================================

updateSchedule :: EventSchedule -> Effect Unit
updateSchedule schedule = GenServer.cast (ByName serverName) \state -> do
  -- TODO run event-check then record schedule change in log
  liftEffect $ updateScheduleImpl schedule
  pure $ GenServer.return state

-- TODO
--reportEventState :: EventId -> EventState -> Effect Unit
--removeSchedule :: String -> Effect Unit
--querySchedule :: Effect (Array EventSchedule)
--queryComingEventsWithin :: String -> Duration -> Effect (Array DateTime)
--queryComingEventsCount :: String -> Int -> Effect (Array DateTime)
--queryHistoricalEventsWithin :: String -> Duration -> Effect (Array DateTime)
--queryHistoricalEventsCount :: String -> Int -> Effect (Array DateTime)

-- ===================================================================
-- CallBacks
-- ===================================================================

startLink :: EventSchedulerStartArgs -> Effect (StartLinkResult (ServerPid Unit Unit Msg State))
startLink args = GenServer.startLink $ (GenServer.defaultSpec $ init args) { name = Just serverName, handleInfo = Just handleInfo }

init :: EventSchedulerStartArgs -> GenServer.InitFn Unit Unit Msg State
init _args = do
  liftEffect $ initDb
  pure $ InitOk {}

handleInfo :: GenServer.InfoFn Unit Unit Msg State
handleInfo msg state =
  case msg of
    Hello -> do
      liftEffect $ Console.log "hello info"
      pure $ GenServer.return state

-- ===================================================================
-- Internal functions
-- ===================================================================

serverName :: RegistryName (ServerType Unit Unit Msg State)
serverName = Local $ atom "event_scheduler"

foreign import initDb :: Effect Unit

foreign import updateScheduleImpl :: EventSchedule -> Effect Unit

-- TODO
-- eventCheck
