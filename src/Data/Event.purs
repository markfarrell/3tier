module Data.Event
  ( Event(..)
  , EventType(..)
  , SessionID
  , FeatureID
  , InstanceID
  , SourceID
  , DestinationID
  , module Data.Event.Class
  , eventTypes
  , foreignEvent
  ) where

import Prelude

import Foreign (Foreign)

import Unsafe.Coerce (unsafeCoerce)

import FFI.Date (Date)
import FFI.JSON (stringify) as JSON
import FFI.UUID (UUID)

import Data.Event.Class (class EventCategory, class EventID, eventCategories, eventIDs)

data EventType = Success | Failure

type SessionID     = UUID
type FeatureID     = UUID
type InstanceID    = UUID 
type SourceID      = UUID
type DestinationID = UUID

data Event a b = Event (EventCategory a => EventID b =>
  { eventCategory :: a
  , eventType     :: EventType
  , eventID       :: b
  , sessionID     :: SessionID
  , featureID     :: FeatureID
  , instanceID    :: InstanceID
  , sourceID      :: SourceID
  , destinationID :: DestinationID
  , startTime     :: Date
  , duration      :: Int
  , endTime       :: Date
  }) 

instance showEventType :: Show EventType where
  show Success = "SUCCESS"
  show Failure = "FAILURE"

instance showEvent :: (EventCategory a, EventID b) => Show (Event a b) where
  show = JSON.stringify <<< foreignEvent

derive instance eqEventType :: Eq EventType

derive instance eqEvent :: (EventCategory a, EventID b) => Eq (Event a b)

eventTypes :: Array EventType
eventTypes = [ Success, Failure ]

foreignEvent :: forall a b. EventCategory a => EventID b => Event a b -> Foreign
foreignEvent (Event x) = unsafeCoerce $
  { eventCategory : show x.eventCategory
  , eventType     : show x.eventType
  , eventID       : show x.eventID
  , sessionID     : show x.sessionID
  , featureID     : show x.featureID
  , instanceID    : show x.instanceID
  , sourceID      : show x.sourceID
  , destinationID : show x.destinationID
  , startTime     : show x.startTime
  , duration      : show x.duration
  , endTime       : show x.endTime
  }
