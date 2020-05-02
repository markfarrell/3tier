module Data.Traffic
  ( Event(..)
  , EventCategory(..)
  , EventType(..)
  , EventID(..)
  , EventURI(..) 
  ) where

import Data.Event (Event, Entity) as Event
import Data.Flow as Flow

data EventCategory = In | Out
                       
data EventType     = Success | Failure

type EventID       = Event.Entity

type EventURI      = Flow.Event

type Event         = Event.Event EventCategory EventType EventID EventURI
