module Effect.Traffic
  ( random
  ) where

import Prelude

import Effect (Effect)

import Data.Port as Port
import Data.Traffic as Traffic 

import Data.Event (eventCategories, eventIDs) as Event

import Effect.Array (random) as Array
import Effect.Event (random) as Event

random :: Effect Traffic.Event
random = Event.random eventCategory eventID
  where
    eventCategory = Array.random Traffic.In                      $ Event.eventCategories
    eventID       = Array.random (Traffic.EventID $ Port.port 0) $ Event.eventIDs
