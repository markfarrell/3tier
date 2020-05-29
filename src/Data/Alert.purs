module Data.Alert
  ( Event(..)
  , EventCategory(..)
  , EventID(..)
  ) where

import Prelude

import Foreign.Class (class Marshall) as F
import Foreign.Coerce (coerce) as F

import Data.Event as E
import Data.EventType (EventType)

data EventCategory = Anomalous

data EventID = Risk

type Event = E.Event EventCategory EventType EventID

instance showEventCategoryAlert :: Show EventCategory where
  show Anomalous = "ANOMALOUS"

instance showEventIDAlert :: Show EventID where
  show Risk = "RISK"

instance marshallEventCategory :: F.Marshall EventCategory where
  marshall = F.coerce <<< show

instance marshallEventID :: F.Marshall EventID where
  marshall = F.coerce <<< show

derive instance eqEventCategoryAlert :: Eq EventCategory

derive instance eqEventIDAlert :: Eq EventID

instance eventCategoryAlert :: E.EventCategory EventCategory where
  eventCategories = [ Anomalous ]

instance eventIDAlert :: E.EventID EventID where
  eventIDs = [ Risk ]
