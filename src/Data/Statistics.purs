module Data.Statistics
  ( EventCategory(..)
  , EventType(..)
  , EventID(..)
  , Event(..)
  , EventURI(..)
  , eventCategories
  , eventIDs
  , eventTypes
  ) where

import Prelude

import Foreign (Foreign)

import Data.Foldable (foldl)

import Data.Event as Event

import FFI.JSON as JSON

import Unsafe.Coerce (unsafeCoerce)

data EventCategory = Source | Duration

data EventType = Success | Failure

data EventID = Alert | Audit | Anomalous | Flow | Statistics | Linux | Windows

data EventURI = EventURI
  { min           :: Int
  , max           :: Int
  , sum           :: Int
  , total         :: Int
  , average       :: Int
  , variance      :: Int 
  }

data Event = Event
  { eventCategory :: EventCategory
  , eventType     :: EventType
  , eventID       :: EventID
  , eventTime     :: Event.EventTime
  , eventSource   :: Event.EventSource
  , eventURI      :: EventURI
  }

instance showEventStatistics :: Show Event where
  show = uri

instance showEventCategoryStatistics :: Show EventCategory where
  show Source   = "SOURCE"
  show Duration = "DURATION"

instance showEventTypeStatistics :: Show EventType where
  show Success = "SUCCESS"
  show Failure = "FAILURE"

instance showEventIDStatistics :: Show EventID where
  show Alert      = "ALERT"
  show Audit      = "AUDIT"
  show Anomalous  = "ANOMALOUS"
  show Flow       = "FLOW"
  show Statistics = "STATISTICS"
  show Linux      = "LINUX"
  show Windows    = "WINDOWS"

instance showEventURIStatistics :: Show EventURI where
  show (EventURI x) = JSON.stringify $ unsafeCoerce x

instance eqEventCategoryStatistics :: Eq EventCategory where
  eq Source Source     = true
  eq Duration Duration = true
  eq _      _          = false

instance eqEventTypeStatistics :: Eq EventType where
  eq Success Success = true
  eq Failure Failure = true
  eq _       _       = false

instance eqEventIDStatistics :: Eq EventID where
  eq Alert      Alert       = true 
  eq Audit      Audit       = true
  eq Anomalous  Anomalous   = true
  eq Flow       Flow        = true
  eq Statistics Statistics  = true
  eq Linux      Linux       = true
  eq Windows    Windows     = true
  eq _          _           = false

instance eqEventURIStatistics :: Eq EventURI where
  eq (EventURI x) (EventURI y) = foldl (&&) true comparison
    where
      comparison =
        [ eq x.min y.min
        , eq x.max y.max
        , eq x.sum y.sum
        , eq x.total y.total
        , eq x.average  y.average
        , eq x.variance y.variance
        ]

instance eqEventStatistics :: Eq Event where
  eq (Event x) (Event y) = foldl (&&) true comparison
    where
      comparison = 
        [ eq x.eventCategory y.eventCategory
        , eq x.eventType     y.eventType
        , eq x.eventID       y.eventID
        , eq x.eventSource   y.eventSource
        , eq x.eventTime     y.eventTime
        , eq x.eventURI      y.eventURI
        ]

eventCategories :: Array EventCategory
eventCategories = [ Source, Duration ]

eventIDs :: Array EventID
eventIDs = [ Alert, Audit, Anomalous, Flow, Statistics, Linux, Windows ]

eventTypes :: Array EventType
eventTypes = [ Success, Failure ]

foreignURI :: EventURI -> Foreign
foreignURI (EventURI x) = unsafeCoerce $
  { min      : show x.min
  , max      : show x.max
  , sum      : show x.sum
  , total    : show x.total
  , average  : show x.average
  , variance : show x.variance
  }

uri :: Event -> String
uri (Event event') = JSON.stringify $ unsafeCoerce $
  { eventCategory : show event'.eventCategory
  , eventType     : show event'.eventType
  , eventID       : show event'.eventID
  , eventTime     : Event.foreignEventTime   $ event'.eventTime
  , eventSource   : Event.foreignEventSource $ event'.eventSource
  , eventURI      : foreignURI event'.eventURI
  }
