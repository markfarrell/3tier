module Effect.EventType
  ( random
  ) where

import Prelude

import Effect (Effect)

import Data.Event as E
import Data.EventType (EventType(..))

import Effect.Array (random) as Array

random :: Effect EventType
random = Array.random Success $ E.eventTypes
