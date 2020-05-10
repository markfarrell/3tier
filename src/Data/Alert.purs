module Data.Alert
  ( EventCategory(..)
  , EventType(..)
  , EventID(..)
  , EventURI
  , Event(..)
  , eventCategories
  , eventIDs
  , eventTypes
  ) where

import Prelude

import Data.Event as Event

import FFI.JSON as JSON

import FFI.UUID (UUID)

import Unsafe.Coerce (unsafeCoerce)

data EventCategory = Source | Time

data EventType = Success | Failure

data EventID = Audit

type EventURI = UUID

data Event = Event
  { eventCategory :: EventCategory
  , eventType     :: EventType
  , eventID       :: EventID
  , eventURI      :: EventURI
  , eventSource   :: Event.EventSource
  , eventTime     :: Event.EventTime
  }

instance showEventAlert :: Show Event where
  show = uri

instance showEventCategoryAlert :: Show EventCategory where
  show Source = "SOURCE"
  show Time   = "TIME"

instance showEventIDAlert :: Show EventID where
  show Audit = "AUDIT"

instance showEventTypeAlert :: Show EventType where
  show Success = "SUCCESS"
  show Failure = "FAILURE"

instance eqEventTypeAlert :: Eq EventType where
  eq Success Success = true
  eq Failure Failure = true
  eq _     _         = false

instance eqEventCategoryAlert :: Eq EventCategory where
  eq Source  Source = true
  eq Time    Time   = true
  eq _     _        = false

instance eqEventIDAlert :: Eq EventID where
  eq Audit Audit = true


instance eqEventAlert :: Eq Event where
  eq (Event x) (Event y) = (x == y)

eventCategories :: Array EventCategory
eventCategories = [ Source, Time ]

eventIDs :: Array EventID
eventIDs = [ Audit ]
  
eventTypes :: Array EventType
eventTypes = [ Success, Failure ]

uri :: Event -> String
uri (Event event') = JSON.stringify $ unsafeCoerce $
  { eventCategory : show event'.eventCategory
  , eventType     : show event'.eventType
  , eventID       : show event'.eventID
  , eventSource   : Event.foreignEventSource event'.eventSource
  , eventTime     : Event.foreignEventTime event'.eventTime
  , eventURI      : show event'.eventURI
  }
