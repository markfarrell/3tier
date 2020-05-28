module Data.Event.Feature
  ( Feature
  , marshall
  ) where

import Prelude

import Foreign (Foreign)
import Unsafe.Coerce (unsafeCoerce)

import Data.Event.Class (class EventCategory, class EventType, class EventID)

{-- Heterogenous feature record type. --}
type Feature a b c = (EventCategory a => EventType b => EventID c =>
  { eventCategory :: a 
  , eventType     :: b
  , eventID       :: c
  })

{-- Homogenous feature record type. --}
type Representation a =
  { eventCategory :: a
  , eventType     :: a
  , eventID       :: a
  }

{-- todo(?):
  class (Heterogenous a, Homogenous b) <= Representable a b where
    marshall :: a -> b 
--}

marshall :: forall a b c. EventCategory a => EventType b => EventID c => Feature a b c -> Representation Foreign
marshall x =
  { eventCategory : marshall' x.eventCategory
  , eventType     : marshall'' x.eventType
  , eventID       : marshall''' x.eventID
  }
       
marshall' :: forall a. EventCategory a => a -> Foreign
marshall' = unsafeCoerce <<< show 

marshall'' :: forall a. EventType a => a -> Foreign
marshall'' = unsafeCoerce <<< show 

marshall''' :: forall a. EventID a => a -> Foreign
marshall''' = unsafeCoerce <<< show 
