module Data.Report
  ( EventCategory(..)
  , EventType(..)
  , EventID(..)
  , Event(..)
  , eventCategories
  , eventIDs
  ) where

import Prelude

import Data.Foldable (foldl)

import FFI.JSON as JSON

import Unsafe.Coerce (unsafeCoerce)

data EventCategory = Source | Duration | Unique

data EventType = Success | Failure

data EventID = Alert | Audit | Anomalous | Flow | Report | Linux | Windows

data Event = Event
  { eventCategory :: EventCategory
  , eventType     :: EventType
  , eventID       :: EventID
  , min           :: Int
  , max           :: Int
  , sum           :: Int
  , total         :: Int
  , average       :: Int
  , variance      :: Int 
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

instance eqEventCategoryReport :: Eq EventCategory where
  eq Source Source     = true
  eq Duration Duration = true
  eq Unique Unique     = true
  eq _      _          = false

instance eqEventTypeReport :: Eq EventType where
  eq Success Success = true
  eq Failure Failure = true
  eq _       _       = false

instance eqEventIDReport :: Eq EventID where
  eq Alert     Alert     = true 
  eq Audit     Audit     = true
  eq Anomalous Anomalous = true
  eq Flow      Flow      = true
  eq Report    Report    = true
  eq Linux     Linux     = true
  eq Windows   Windows   = true
  eq _         _         = false

instance eqEventReport :: Eq Event where
  eq (Event x) (Event y) = foldl (&&) true comparison
    where
      comparison = 
        [ eq x.eventCategory y.eventCategory
        , eq x.eventType  y.eventType
        , eq x.eventID  y.eventID
        , eq x.min  y.min
        , eq x.max  y.max
        , eq x.sum  y.sum
        , eq x.total  y.total
        , eq x.average  y.average
        ]

eventCategories :: Array EventCategory
eventCategories = [ Source, Duration, Unique ]

eventIDs :: Array EventID
eventIDs = [ Alert, Audit, Anomalous, Flow, Report, Linux, Windows ]

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




