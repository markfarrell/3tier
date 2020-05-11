module Data.Event
  ( Event(..)
  , EventType(..)
  , EventURI
  , EventSource(..)
  , EventTime(..)
  , module Data.Event.Class
  , eventTypes
  , foreignEventURI
  , foreignEventTime
  , foreignEventSource
  ) where

import Prelude

import Foreign (Foreign)

import Unsafe.Coerce (unsafeCoerce)

import FFI.Date (Date)
import FFI.JSON (stringify) as JSON
import FFI.UUID (UUID)

import Data.Event.Class (class EventCategory, class EventID, eventCategories, eventIDs)

data EventType = Success | Failure

type EventURI  = UUID 

data EventTime = EventTime
  { startTime :: Date
  , duration  :: Int
  , endTime   :: Date 
  }

type EventSource = UUID

data Event a b = Event
  { eventCategory :: a
  , eventType     :: EventType
  , eventID       :: b
  , eventURI      :: EventURI
  , eventTime     :: EventTime
  , eventSource   :: EventSource
  } 

instance showEventType :: Show EventType where
  show Success = "SUCCESS"
  show Failure = "FAILURE"

instance showEvent :: (Show a, Show b) => Show (Event a b) where
  show = JSON.stringify <<< foreignEvent

derive instance eqEventType :: Eq EventType

derive instance eqEventTime :: Eq EventTime

derive instance eqEvent :: (Eq a, Eq b) => Eq (Event a b)

eventTypes :: Array EventType
eventTypes = [ Success, Failure ]

foreignEventTime :: EventTime -> Foreign
foreignEventTime (EventTime x) = unsafeCoerce $
  { startTime : show x.startTime
  , duration  : show x.duration
  , endTime   : show x.endTime
  }

foreignEventSource :: EventSource -> Foreign
foreignEventSource = unsafeCoerce <<< show

foreignEventURI :: EventURI -> Foreign
foreignEventURI = unsafeCoerce <<< show

foreignEvent :: forall a b. Show a => Show b => Event a b -> Foreign
foreignEvent (Event x) = unsafeCoerce $
  { eventCategory : show x.eventCategory
  , eventType     : show x.eventType
  , eventID       : show x.eventID
  , eventSource   : foreignEventSource x.eventSource
  , eventURI      : foreignEventURI x.eventURI
  , eventTime     : foreignEventTime x.eventTime
  }
