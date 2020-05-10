module Data.Traffic
  ( Event(..)
  , EventCategory(..)
  , EventID
  , eventCategories
  , eventIDs
  ) where

import Prelude

import Data.Array as Array

import Unsafe.Coerce (unsafeCoerce)

import Data.Port (Port)
import Data.Event as Event

data EventCategory = In | Out
                       
type EventID = Port

type Event = Event.Event EventCategory EventID

instance showEventCategoryTraffic :: Show EventCategory where 
  show In  = "IN"
  show Out = "OUT"

derive instance eqEventCategoryTraffic :: Eq EventCategory

eventCategories :: Array EventCategory
eventCategories = [In, Out]

eventIDs :: Array EventID
eventIDs = unsafeCoerce <$> Array.range 0 65535
