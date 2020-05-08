module Data.Alert
  ( EventCategory(..)
  , EventType(..)
  , EventID(..)
  , EventURI
  , Event(..)
  , eventCategories
  , eventIDs
  ) where

import Prelude

import Data.Event as Event

import FFI.JSON as JSON

import FFI.UUID (UUID)

import Unsafe.Coerce (unsafeCoerce)

data EventCategory = Audit

data EventType = Success | Failure

data EventID = Source | Time

type EventURI = UUID

data Event = Event
  { eventCategory :: EventCategory
  , eventType     :: EventType
  , eventID       :: EventID
  , eventURI      :: EventURI
  , eventSource   :: Event.Source
  , eventTime     :: Event.Time
  }

instance showEventAlert :: Show Event where
  show = uri

instance showEventCategoryAlert :: Show EventCategory where
  show Audit = "AUDIT"

instance showEventTypeAlert :: Show EventType where
  show Success = "SUCCESS"
  show Failure = "FAILURE"

instance showEventIDAlert :: Show EventID where
  show Source = "SOURCE"
  show Time   = "TIME"

eventCategories :: Array EventCategory
eventCategories = [ Audit ]

eventIDs :: Array EventID
eventIDs = [ Source, Time ]

uri :: Event -> String
uri (Event event') = JSON.stringify $ unsafeCoerce $
  { eventCategory : show event'.eventCategory
  , eventType     : show event'.eventType
  , eventID       : show event'.eventID
  , eventSource   : Event.foreignSource event'.eventSource
  , eventTime     : Event.foreignTime event'.eventTime
  }
