module Data.Audit
  ( EventType(..)
  , EventCategory(..)
  , EventID(..)
  , EventURI
  , Event(..)
  , ReportType(..)
  , eventCategories
  , eventIDs
  ) where

import Prelude

import Data.IPv4 (IPv4)

import Data.Schema (Schema)
import Data.Schema as Schema

import FFI.Date (Date)
import FFI.JSON as JSON

import Unsafe.Coerce (unsafeCoerce)

data EventType = Success | Failure

data EventID = Forward Schema | Report Schema | Anomalous 

data EventCategory = Tier1 | Tier2 | Tier3

type EventURI = String

data ReportType = Source | Duration

data Event = Event 
  { sIP           :: IPv4
  , sPort         :: Int 
  , eventType     :: EventType
  , eventCategory :: EventCategory
  , eventID       :: EventID
  , startTime     :: Date
  , duration      :: Int
  , endTime       :: Date
  }

instance showEventWindows :: Show Event where
  show = uri

instance showEventTypeAudit :: Show EventType where
  show Success = "SUCCESS"
  show Failure = "FAILURE"

instance showEventIDAudit :: Show EventID where
  show (Forward Schema.Audit)       = "FORWARD-AUDIT"
  show (Forward Schema.Alert)       = "FORWARD-ALERT"
  show (Forward Schema.Flow)        = "FORWARD-FLOW"
  show (Forward Schema.Linux)       = "FORWARD-LINUX"
  show (Forward Schema.Statistics)  = "FORWARD-STATISTICS"
  show (Forward Schema.Windows)     = "FORWARD-WINDOWS"
  show (Report  Schema.Audit)       = "REPORT-AUDIT"
  show (Report  Schema.Alert)       = "REPORT-ALERT"
  show (Report  Schema.Flow)        = "REPORT-FLOW"
  show (Report  Schema.Linux)       = "REPORT-LINUX"
  show (Report  Schema.Statistics)  = "REPORT-STATISTICS"
  show (Report  Schema.Windows)     = "REPORT-WINDOWS"
  show (Anomalous)                  = "ANOMALOUS"

instance showEventCategory :: Show EventCategory where
  show Tier1 = "TIER-1"
  show Tier2 = "TIER-2"
  show Tier3 = "TIER-3"

instance eqEventTypeAudit :: Eq EventType where
  eq Success Success = true
  eq Failure Failure = true
  eq _       _       = false

eventCategories :: Array EventCategory
eventCategories = [Tier1, Tier2, Tier3]

eventIDs :: Array EventID
eventIDs =
  [ Anomalous
  , Forward Schema.Audit
  , Forward Schema.Alert
  , Forward Schema.Flow
  , Forward Schema.Linux
  , Forward Schema.Statistics
  , Forward Schema.Windows
  , Report Schema.Audit
  , Report Schema.Alert
  , Report Schema.Flow
  , Report Schema.Statistics
  , Report Schema.Linux
  , Report Schema.Windows
  ]

uri :: Event -> String
uri (Event event') = JSON.stringify $ unsafeCoerce $
  { sIP           : show event'.sIP
  , sPort         : event'.sPort
  , eventType     : show event'.eventType
  , eventCategory : show event'.eventCategory
  , eventID       : show event'.eventID
  , startTime     : show event'.startTime
  , duration      : event'.duration
  , endTime       : show event'.endTime
  }
