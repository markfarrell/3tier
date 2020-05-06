module Data.Traffic
  ( Event(..)
  , EventCategory(..)
  , EventType(..)
  , EventID(..)
  , EventURI(..) 
  ) where

import Data.Event (Event) as Event
import Data.Flow as Flow

data EventCategory = In | Out
                       
data EventType     = Success | Failure

data EventID       = HTTP | FTP | SSH | MicrosoftDS | WinRM -- | ...

type EventURI      = Flow.Event

type Event         = Event.Event EventCategory EventType EventID EventURI
