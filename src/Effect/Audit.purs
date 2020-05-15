module Effect.Audit
  ( random
  ) where

import Prelude

import Effect (Effect)

import Data.Audit as Audit 

import Data.Event (eventCategories, eventIDs) as Event

import Effect.Array (random) as Array
import Effect.Event (random) as Event

random :: Effect Audit.Event
random = Event.random eventCategory eventID
  where
    eventCategory = Array.random Audit.Forward $ Event.eventCategories
    eventID       = Array.random Audit.Alert   $ Event.eventIDs
