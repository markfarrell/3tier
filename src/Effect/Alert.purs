module Effect.Alert
  ( random
  ) where

import Prelude

import Effect (Effect)

import Data.Alert as Alert 

import Data.Event (eventCategories, eventIDs) as Event

import Effect.Array (random) as Array
import Effect.Event (random) as Event
import Effect.EventType (random) as EventType

random :: Effect Alert.Event
random = Event.random eventCategory EventType.random eventID
  where
    eventCategory = Array.random Alert.Anomalous $ Event.eventCategories
    eventID       = Array.random Alert.Risk      $ Event.eventIDs
