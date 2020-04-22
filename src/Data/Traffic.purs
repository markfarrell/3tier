module Data.Traffic
  ( Event(..)
  , EventCategory(..)
  , EventType(..)
  , EventID(..)
  , EventURI(..) 
  ) where

import Data.IPv4 (IPv4)

import Data.Event (Event) as Data
import Data.Flow as Flow

data EventCategory = In | Out
                       
data EventType     = Success | Failure

data EventID       = Host IPv4

data EventURI      = Flow Flow.Event

type Event         = Data.Event EventCategory EventType EventID EventURI
