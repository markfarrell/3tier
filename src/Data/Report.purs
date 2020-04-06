module Data.Report
  ( EventCategory(..)
  , EventType(..)
  , EventID(..)
  , Event(..)
  ) where

import Prelude

import FFI.JSON as JSON

import Unsafe.Coerce (unsafeCoerce)

data EventCategory = Source | Duration | Unique

data EventType = Success | Failure

data EventID = Alert | Audit | Anomalous | Flow | Report | Linux | Windows

data Event = Event
  { eventCategory :: EventCategory
  , eventType     :: EventType
  , eventID       :: EventID
  , min           :: Number
  , max           :: Number
  , sum           :: Number
  , total         :: Number
  , average       :: Number
  , variance      :: Number 
  }

instance showEventReport :: Show Event where
  show = uri

instance showEventCategoryReport :: Show EventCategory where
  show Source   = "SOURCE"
  show Duration = "DURATION"
  show Unique   = "UNIQUE"

instance showEventTypeReport :: Show EventType where
  show Success = "SUCCESS"
  show Failure = "FAILURE"

instance showEventIDReport :: Show EventID where
  show Alert     = "ALERT"
  show Audit     = "AUDIT"
  show Anomalous = "ANOMALOUS"
  show Flow      = "FLOW"
  show Report    = "REPORT"
  show Linux     = "LINUX"
  show Windows   = "WINDOWS"

uri :: Event -> String
uri (Event event') = JSON.stringify $ unsafeCoerce $
  { eventCategory : show event'.eventCategory
  , eventType     : show event'.eventType
  , eventID       : show event'.eventID
  , min           : show event'.min
  , max           : show event'.max
  , sum           : show event'.sum
  , total         : show event'.total
  , average       : show event'.average
  , variance       : show event'.variance
  }




