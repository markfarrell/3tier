module Data.Audit
  ( EventType(..)
  , EventCategory(..)
  , EventID(..)
  , EventURI
  , Event(..)
  , eventCategories
  , eventIDs
  , eventTypes
  ) where

import Prelude

import Data.Event as Event

import FFI.UUID (UUID)
import FFI.JSON as JSON

import Unsafe.Coerce (unsafeCoerce)

data EventType = Success | Failure

data EventID = Alert | Audit | Traffic | Windows | Linux | Statistics | Risk

data EventCategory = Forward | Report | Anomalous

type EventURI = UUID

data Event = Event 
  { eventCategory :: EventCategory
  , eventType     :: EventType
  , eventID       :: EventID
  , eventTime     :: Event.EventTime
  , eventSource   :: Event.EventSource
  , eventURI      :: EventURI
  }

instance showEventWindows :: Show Event where
  show = uri

instance showEventTypeAudit :: Show EventType where
  show Success = "SUCCESS"
  show Failure = "FAILURE"

instance showEventIDAudit :: Show EventID where
  show (Alert)        = "ALERT"
  show (Audit)        = "AUDIT"
  show (Traffic)      = "TRAFFIC"
  show (Linux)        = "LINUX"
  show (Statistics)   = "STATISTICS"
  show (Windows)      = "WINDOWS"
  show (Risk)         = "RISK"

instance showEventCategory :: Show EventCategory where
  show Forward   = "FORWARD"
  show Report    = "REPORT"
  show Anomalous = "ANOMALOUS"

instance eqEventCategoryAudit :: Eq EventCategory where
  eq Forward Forward      = true
  eq Report  Report       = true
  eq Anomalous Anomalous  = true
  eq _     _              = false

instance eqEventTypeAudit :: Eq EventType where
  eq Success Success = true
  eq Failure Failure = true
  eq _       _       = false

instance eqEventIDAudit :: Eq EventID where
  eq (Alert)      (Alert)      = true
  eq (Audit)      (Audit)      = true
  eq (Traffic)    (Traffic)    = true
  eq (Linux)      (Linux)      = true
  eq (Statistics) (Statistics) = true
  eq (Windows)    (Windows)    = true
  eq (Risk)       (Risk)       = true
  eq _           _             = false

instance eqEventAudit :: Eq Event where
  eq (Event x) (Event y) = (x == y)

eventCategories :: Array EventCategory
eventCategories = [Forward, Report, Anomalous]

eventIDs :: Array EventID
eventIDs =
  [ Audit
  , Alert
  , Traffic
  , Windows
  , Linux
  , Statistics
  , Risk
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
  , eventTime     : Event.foreignEventTime event'.eventTime
  }
