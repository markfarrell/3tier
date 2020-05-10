module Data.Event
  ( Event(..)
  , EventType(..)
  , EventURI
  , EventSource(..)
  , EventTime(..)
  , foreignEventType
  , foreignEventURI
  , foreignEventTime
  , foreignEventSource
  ) where

import Prelude

import Data.Foldable (foldl)

import Foreign (Foreign)

import Unsafe.Coerce (unsafeCoerce)

import FFI.Date (Date)
import FFI.JSON (stringify) as JSON
import FFI.UUID (UUID)

import Data.IPv4 (IPv4)
import Data.Port (Port)

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

derive instance eqEventType :: Eq EventType

derive instance eqEventTime :: Eq EventTime

derive instance eqEvent :: (Eq a, Eq b) => Eq (Event a b)

foreignEventTime :: EventTime -> Foreign
foreignEventTime (EventTime x) = unsafeCoerce $
  { startTime : show x.startTime
  , duration  : show x.duration
  , endTime   : show x.endTime
  }

foreignEventSource :: EventSource -> Foreign
foreignEventSource = unsafeCoerce <<< show

foreignEventType :: EventType -> Foreign
foreignEventType = unsafeCoerce <<< show

foreignEventURI :: EventURI -> Foreign
foreignEventURI = unsafeCoerce <<< show
