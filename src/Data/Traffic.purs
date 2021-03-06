module Data.Traffic
  ( Event(..)
  , EventCategory(..)
  , EventID(..)
  ) where

import Prelude

import Data.Array as Array

import Unsafe.Coerce (unsafeCoerce)

import Foreign.Class (class Marshall) as F
import Foreign.Coerce (coerce) as F

import Data.Port (Port)
import Data.Event as E
import Data.EventType (EventType)

data EventCategory = In | Out
                       
data EventID = EventID Port

type Event = E.Event EventCategory EventType EventID

instance showEventCategoryTraffic :: Show EventCategory where 
  show In  = "IN"
  show Out = "OUT"

instance showEventIDTraffic :: Show EventID where
  show (EventID port) = show port

derive instance eqEventCategoryTraffic :: Eq EventCategory

derive instance eqEventIDTraffic :: Eq EventID

instance marshallEventCategory :: F.Marshall EventCategory where
  marshall = F.coerce <<< show

instance marshallEventID :: F.Marshall EventID where
  marshall = F.coerce <<< show

instance eventCategoryTraffic :: E.EventCategory EventCategory where
  eventCategories = [ In, Out ]

instance eventIDTraffic :: E.EventID EventID where
  eventIDs = EventID <$> ports
    where ports = unsafeCoerce <$> Array.range 0 65535
