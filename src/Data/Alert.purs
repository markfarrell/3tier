module Data.Alert
  ( Event(..)
  , EventCategory(..)
  , EventID(..)
  ) where

import Prelude

import Data.Event as E
import Data.EventType (EventType)

data EventCategory = Anomalous

data EventID = Risk

type Event = E.Event EventCategory EventType EventID

instance showEventCategory :: Show EventCategory where
  show Anomalous = "ANOMALOUS"

instance showEventIDAlert :: Show EventID where
  show Risk = "RISK"

derive instance eqEventCategoryAlert :: Eq EventCategory

derive instance eqEventIDAlert :: Eq EventID

instance eventCategoryAlert :: E.EventCategory EventCategory where
  eventCategories = [ Anomalous ]

instance eventIDAlert :: E.EventID EventID where
  eventIDs = [ Risk ]
