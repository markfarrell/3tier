module Data.Statistics
  ( Event(..)
  , EventCategory(..)
  , EventID(..)
  , EventURI(..)
  , eventCategories
  , eventIDs
  , minimum
  , maximum
  , sum
  , total
  , average
  , variance
  ) where

import Prelude

import Foreign (Foreign)

import Data.Event as Event

import FFI.JSON as JSON

import Unsafe.Coerce (unsafeCoerce)

data EventCategory = Source | Time

data EventID = Alert | Audit | Traffic | Linux | Windows

data EventURI = EventURI
  { min           :: Int
  , max           :: Int
  , sum           :: Int
  , total         :: Int
  , average       :: Int
  , variance      :: Int 
  }

{-- todo: revise format (not an event?) --}
data Event = Event
  { eventCategory :: EventCategory
  , eventType     :: Event.EventType
  , eventID       :: EventID
  , eventTime     :: Event.EventTime
  , eventSource   :: Event.EventSource
  , eventURI      :: EventURI
  }

instance showEventStatistics :: Show Event where
  show = uri

instance showEventCategoryStatistics :: Show EventCategory where
  show Source = "SOURCE"
  show Time   = "TIME"

instance showEventIDStatistics :: Show EventID where
  show Alert      = "ALERT"
  show Audit      = "AUDIT"
  show Traffic    = "TRAFFIC"
  show Linux      = "LINUX"
  show Windows    = "WINDOWS"

instance showEventURIStatistics :: Show EventURI where
  show (EventURI x) = JSON.stringify $ unsafeCoerce x

derive instance eqEventCategoryStatistics :: Eq EventCategory
derive instance eqEventIDStatistics       :: Eq EventID
derive instance eqEventURIStatistics      :: Eq EventURI
derive instance eqEventStatistics         :: Eq Event 

eventCategories :: Array EventCategory
eventCategories = [ Source, Time ]

eventIDs :: Array EventID
eventIDs = [ Alert, Audit, Traffic, Linux, Windows ]

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
uri (Event x) = JSON.stringify $ unsafeCoerce $
  { eventCategory : show x.eventCategory
  , eventType     : show x.eventType
  , eventID       : show x.eventID
  , eventTime     : Event.foreignEventTime   $ x.eventTime
  , eventSource   : Event.foreignEventSource $ x.eventSource
  , eventURI      : foreignURI x.eventURI
  }

minimum :: Event -> Int
minimum (Event x) = case x.eventURI of (EventURI y) -> y.min

maximum :: Event -> Int
maximum (Event x) = case x.eventURI of (EventURI y) -> y.max

sum :: Event -> Int
sum (Event x) = case x.eventURI of (EventURI y) -> y.sum

total :: Event -> Int
total (Event x) = case x.eventURI of (EventURI y) -> y.total

average :: Event -> Int
average (Event x) = case x.eventURI of (EventURI y) -> y.average

variance :: Event -> Int
variance (Event x) = case x.eventURI of (EventURI y) -> y.variance
