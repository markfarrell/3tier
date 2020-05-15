module Effect.Linux
  ( random
  ) where

import Prelude

import Effect (Effect)

import FFI.Number as Number

import Data.Linux as Linux 

import Data.Event (eventCategories) as Event

import Effect.Array (random) as Array
import Effect.Event (random) as Event
import Effect.Range (random) as Range

random :: Effect Linux.Event
random = Event.random eventCategory eventID
  where
    eventCategory = Array.random Linux.DaemonStart $ Event.eventCategories
    eventID       = Linux.EventID <$> Range.random 0 Number.maxSafeInteger
