module Data.Traffic
  ( Event(..)
  , EventCategory(..)
  , EventID
  ) where

import Prelude

import Data.Port (Port)
import Data.Event as Event

data EventCategory = In | Out
                       
type EventID = Port

type Event = Event.Event EventCategory EventID

instance showEventCategoryTraffic :: Show EventCategory where 
  show In  = "IN"
  show Out = "OUT"

derive instance eqEventCategoryTraffic :: Eq EventCategory
