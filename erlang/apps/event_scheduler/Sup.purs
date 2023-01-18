module Sup where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List (nil, (:))
import Pinto (RegistryName(..), StartLinkResult)
import Pinto.Supervisor (ChildShutdownTimeoutStrategy(..), ChildType(..), RestartStrategy(..), Strategy(..), SupervisorPid, SupervisorSpec, spec)
import Pinto.Supervisor as Sup

import EventScheduler as EventScheduler

startLink :: Effect (StartLinkResult SupervisorPid)
startLink = do
  Sup.startLink (Just $ Local $ atom "example_sup") init

init :: Effect SupervisorSpec
init = do
  pure
    { flags:
        { strategy: OneForOne
        , intensity: 1
        , period: Seconds 5.0
        }
    , childSpecs:
        (spec { id: "hello"
              , childType: Worker
              , start: EventScheduler.startLink {}
              , restartStrategy: RestartPermanent
              , shutdownStrategy: ShutdownTimeout $ Milliseconds 5000.0
              })
        : nil
    }
