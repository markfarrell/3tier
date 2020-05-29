module Data.Event.Class
  ( class EventCategory
  , eventCategories
  , class EventType
  , eventTypes
  , class EventID
  , eventIDs
  ) where

import Prelude

import Foreign.Class (class Marshall)

{-- todo: use Data.NonEmpty --}

class (Show a, Eq a, Marshall a) <= EventCategory a where
  eventCategories :: Array a
 
class (Show a, Eq a, Marshall a) <= EventType a where
  eventTypes :: Array a 

class (Show a, Eq a, Marshall a) <= EventID a where
  eventIDs :: Array a
