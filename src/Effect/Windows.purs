module Effect.Windows
  ( random
  ) where

import Prelude

import Effect (Effect)

import Data.Windows as Windows 

import Data.Event (eventCategories) as Event

import Effect.Array (random) as Array
import Effect.Event (random) as Event

random :: Effect Windows.Event
random = do
  eventCategory <- Array.random Windows.AccountLogon $ Event.eventCategories
  event         <- Event.random (pure eventCategory) (eventID eventCategory)
  pure event
  where
    eventID eventCategory = Array.random (Windows.EventID 0) (Windows.eventIDs' eventCategory)
