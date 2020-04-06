module Audit
  ( EventType(..)
  , EventCategory(..)
  , EventID(..)
  , EventURI
  , Event(..)
  , ReportType(..)
  , uri
  ) where

import Prelude

import Data.Foldable (intercalate)

import Data.Schema (Schema)
import Data.Schema as Schema

import FFI.Date (Date)

data EventType = Success | Failure

data EventID = Forward Schema | Report Schema | Anomalous 

data EventCategory = Tier1 | Tier2 | Tier3

type EventURI = String

data ReportType = Source | Duration

data Event = Event 
  { sIP           :: String
  , sPort         :: Int 
  , eventType     :: EventType
  , eventCategory :: EventCategory
  , eventID       :: EventID
  , startTime     :: Date
  , duration      :: Int
  , endTime       :: Date
  }

instance showEventTypeAudit :: Show EventType where
  show Success = "SUCCESS"
  show Failure = "FAILURE"

instance showEventIDAudit :: Show EventID where
  show (Forward Schema.Audit)   = "FORWARD-AUDIT"
  show (Forward Schema.Flow)    = "FORWARD-FLOW"
  show (Forward Schema.Windows) = "FORWARD-WINDOWS"
  show (Report  Schema.Audit)   = "REPORT-AUDIT"
  show (Report  Schema.Flow)    = "REPORT-FLOW"
  show (Report  Schema.Windows) = "REPORT-WINDOWS"
  show (Anomalous)              = "ANOMALOUS"

instance showEventCategory :: Show EventCategory where
  show Tier1 = "TIER-1"
  show Tier2 = "TIER-2"
  show Tier3 = "TIER-3"

instance eqEventTypeAudit :: Eq EventType where
  eq Success Success = true
  eq Failure Failure = true
  eq _       _       = false

uri :: Event -> String
uri (Event event') = intercalate separator $
  [ event'.sIP
  , show event'.sPort
  , show event'.eventType
  , show event'.eventCategory
  , show event'.eventID
  , show event'.duration
  ] 
  where separator = ","
