module Test.Data.Test
  ( Event(..)
  , EventCategory(..)
  , EventType(..)
  , EventID(..)
  , EventState(..)
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (intercalate)

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)

import Effect.Exception (Error)
import Effect.Exception as Exception

import Data.Audit as Audit

data EventCategory = Tier1 | Tier2 | Tier3

data EventType = Local | Replication | Failover

data EventID = Forward | Report 

data EventState = Running | Success | Failure

data Event a = Event
  { eventCategory :: EventCategory
  , eventType     :: EventType
  , eventID       :: EventID
  , eventState    :: EventState
  }

instance showEventTest :: Show (Event a) where
  show (Event test) = intercalate " " ["[" <> show test.eventState <> "]", show test.eventCategory, show test.eventType, show test.eventID]
  
instance showEventCategoryTest :: Show EventCategory where
  show Tier1 = "TIER-01"
  show Tier2 = "TIER-02"
  show Tier3 = "TIER-03"

instance showEventTypeTest :: Show EventType where
  show Local       = "LOCAL"
  show Replication = "REPLICATION"
  show Failover    = "FAILOVER"

instance showEventIDTest :: Show EventID where
  show Forward = "FORWARD"
  show Report  = "REPORT"

instance showEventStateTest :: Show EventState where
  show Running = "RUNNING"
  show Success = "SUCCESS"
  show Failure = "FAILURE"
