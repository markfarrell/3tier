module Data.Event.Class
  ( class EventCategory
  , eventCategories
  , class EventID
  , eventIDs
  ) where

import Prelude

class (Show a, Eq a) <= EventCategory a where
  eventCategories :: Array a
  

class (Show a, Eq a) <= EventID a where
  eventIDs :: Array a

{-- todo: use Data.NonEmpty -- }
