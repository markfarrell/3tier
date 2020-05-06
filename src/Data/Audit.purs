module Data.Audit
  ( EventType(..)
  , EventCategory(..)
  , EventID(..)
  , EventURI
  , Event(..)
  , ReportType(..)
  , eventCategories
  , eventIDs
  , eventTypes
  ) where

import Prelude

import Data.Event as Event

import Data.Schema (Schema)
import Data.Schema as Schema

import FFI.UUID (UUID)
import FFI.JSON as JSON

import Unsafe.Coerce (unsafeCoerce)

data EventType = Success | Failure

data EventID = Forward Schema | Report Schema | Risk | Anomalous

data EventCategory = Tier1 | Tier2 | Tier3

type EventURI = UUID

data ReportType = Source | Duration

data Event = Event 
  { eventCategory :: EventCategory
  , eventType     :: EventType
  , eventID       :: EventID
  , eventTime     :: Event.Time
  , eventSource   :: Event.Source
  , eventURI      :: EventURI
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
  show (Risk)                       = "RISK"
  show (Anomalous)                  = "ANOMALOUS"

instance showEventCategory :: Show EventCategory where
  show Tier1 = "TIER-1"
  show Tier2 = "TIER-2"
  show Tier3 = "TIER-3"

instance eqEventCategoryAudit :: Eq EventCategory where
  eq Tier1 Tier1 = true
  eq Tier2 Tier2 = true
  eq Tier3 Tier3 = true
  eq _     _     = false

instance eqEventTypeAudit :: Eq EventType where
  eq Success Success = true
  eq Failure Failure = true
  eq _       _       = false

instance eqEventIDAudit :: Eq EventID where
  eq (Forward x) (Forward y) = true
  eq (Report  x) (Report  y) = true
  eq (Risk)      (Risk)      = true
  eq (Anomalous) (Anomalous) = true
  eq _           _           = false

instance eqEventAudit :: Eq Event where
  eq (Event x) (Event y) = (x == y)

eventCategories :: Array EventCategory
eventCategories = [Tier1, Tier2, Tier3]

eventIDs :: Array EventID
eventIDs =
  [ Anomalous
  , Risk
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

eventTypes :: Array EventType
eventTypes = [ Success, Failure ]

uri :: Event -> String
uri (Event event') = JSON.stringify $ unsafeCoerce $
  { eventCategory : show event'.eventCategory
  , eventType     : show event'.eventType
  , eventID       : show event'.eventID
  , eventSource   : show event'.eventSource
  , eventURI      : show event'.eventURI
  , eventTime     : Event.foreignTime event'.eventTime
  }
