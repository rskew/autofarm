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
  = { action :: EventId -> Effect Unit
    , runAfter :: Time
    , runBefore :: Time
    , timeoutFromConfirmedStartSeconds :: Int
    , errorHandler :: EventError -> Effect Unit
    , everyDays :: Int
    }

data EventState
  = EventComplete
  | EventNotComplete String

-- ===================================================================
-- API
-- ===================================================================

updateSchedule :: String -> Maybe EventSchedule -> Effect Unit
updateSchedule key maybeSchedule = GenServer.cast (ByName serverName) \state -> do
  case maybeSchedule of
    Nothing -> liftEffect $ removeScheduleImpl key
    Just schedule -> do
      -- TODO run event-check then record schedule change in log
      liftEffect $ updateScheduleImpl key schedule
  pure $ GenServer.return state

-- TODO
--updateEventState :: EventId -> EventState -> Effect Unit
--querySchedule :: String -> Effect (Maybe EventSchedule)
--queryAllSchedules :: Effect (Array EventSchedule)
--queryComingEventsWithin :: String -> Duration -> Effect (Array DateTime)
--queryComingEventsCount :: String -> Int -> Effect (Array DateTime)
--queryHistoricalEventsWithin :: String -> Duration -> Effect (Array DateTime)
--queryHistoricalEventsCount :: String -> Int -> Effect (Array DateTime)

-- ===================================================================
-- Internal API
-- ===================================================================

-- | Check an event schedule for current events that don't have corresponding logs.
-- | If a current event has no corresponding log, then action the event and create the log.
eventCheck :: String -> Effect Unit
-- TODO
eventCheck _ = pure unit

--createEventLog :: String -> DateTime -> EventState -> Effect Unit
---- TODO
--createEventLog _ _ _ = pure unit

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

foreign import updateScheduleImpl :: String -> EventSchedule -> Effect Unit

foreign import removeScheduleImpl :: String -> Effect Unit

-- TODO
-- eventCheck
