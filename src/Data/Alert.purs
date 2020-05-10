module Data.Alert
  ( Event(..)
  , EventCategory(..)
  , EventID(..)
  , eventCategories
  , eventIDs
  ) where

import Prelude

import Data.Event as Event

data EventCategory = Anomalous

data EventID = Risk

type Event = Event.Event EventCategory EventID

instance showEventCategory :: Show EventCategory where
  show Anomalous = "ANOMALOUS"

instance showEventIDAlert :: Show EventID where
  show Risk = "RISK"

derive instance eqEventCategoryAlert :: Eq EventCategory

derive instance eqEventIDAlert :: Eq EventID

eventCategories :: Array EventCategory
eventCategories = [ Anomalous ]

eventIDs :: Array EventID
eventIDs = [ Risk ]
