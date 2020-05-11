module Data.Event.Class
  ( class EventCategory
  , eventCategories
  , class EventID
  , eventIDs
  ) where

import Prelude

class (Show a) <= EventCategory a where
  eventCategories :: Array a
  

class (Show a) <= EventID a where
  eventIDs :: Array a
