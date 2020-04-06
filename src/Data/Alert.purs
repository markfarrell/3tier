module Data.Alert
  ( EventCategory(..)
  , EventType(..)
  , EventID(..)
  , Event(..)
  , eventCategories
  , eventIDs
  ) where

import Prelude

import Data.IPv4 (IPv4)

import FFI.Date (Date)
import FFI.JSON as JSON

import Unsafe.Coerce (unsafeCoerce)

data EventCategory = Min | Max | Sum | Total | Average | Variance

data EventType = Success | Failure

data EventID = Alert | Audit | Anomalous | Flow | Report | Linux | Windows

data Event = Event
  { eventCategory :: EventCategory
  , eventType     :: EventType
  , eventID       :: EventID
  , startTime     :: Date
  , duration      :: Int
  , endTime       :: Date
  , sIP           :: IPv4
  , sPort         :: Int
  }

instance showEventAlert :: Show Event where
  show = uri

instance showEventCategoryAlert :: Show EventCategory where
  show Min      = "MIN"
  show Max      = "MAX"
  show Sum      = "SUM"
  show Total    = "TOTAL"
  show Average  = "AVERAGE"
  show Variance = "VARIANCE"

instance showEventTypeAlert :: Show EventType where
  show Success = "SUCCESS"
  show Failure = "FAILURE"

instance showEventIDAlert :: Show EventID where
  show Alert     = "ALERT"
  show Audit     = "AUDIT"
  show Anomalous = "ANOMALOUS"
  show Flow      = "FLOW"
  show Report    = "REPORT"
  show Linux     = "LINUX"
  show Windows   = "WINDOWS"

eventCategories :: Array EventCategory
eventCategories = [ Min, Max, Sum, Total, Average, Variance ]

eventIDs :: Array EventID
eventIDs = [ Alert, Audit, Anomalous, Flow, Report, Linux, Windows ]

uri :: Event -> String
uri (Event event') = JSON.stringify $ unsafeCoerce $
  { eventCategory : show event'.eventCategory
  , eventType     : show event'.eventType
  , eventID       : show event'.eventID
  , startTime     : show event'.startTime
  , duration      : event'.duration
  , endTime       : show event'.endTime
  , sIP           : show event'.sIP
  , sPort         : show event'.sPort
  }




