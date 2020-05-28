module Data.Event
  ( Event(..)
  , SourceID
  , SessionID
  , DestinationID
  , LogID
  , SchemaID
  , FeatureID
  , InstanceID
  , module Data.Event.Class
  , module Data.Event.EventType
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

import Data.Event.EventType (EventType(..))

type SourceID      = UUID
type SessionID     = UUID
type DestinationID = UUID
type LogID         = UUID
type SchemaID      = UUID

type FeatureID     = UUID
type InstanceID    = UUID 

data Event a b = Event (EventCategory a => EventID b =>
  { eventCategory :: a
  , eventType     :: EventType
  , eventID       :: b
  , sourceID      :: SourceID
  , sessionID     :: SessionID
  , destinationID :: DestinationID
  , logID         :: LogID
  , schemaID      :: SchemaID
  , featureID     :: FeatureID
  , instanceID    :: InstanceID
  , startTime     :: Date
  , duration      :: Int
  , endTime       :: Date
  }) 

{-- todo: see https://github.com/markfarrell/3iter/issues/5 --}

type Feature a b = (EventCategory a => EventID b =>
  { eventCategory :: a 
  , eventType     :: EventType
  , eventID       :: b
  })

type Identifier =
  { sourceID      :: SourceID
  , sessionID     :: SessionID
  , destinationID :: DestinationID
  , logID         :: LogID
  , schemaID      :: SchemaID
  }

type Checksum =
  { featureID  :: FeatureID
  , instanceID :: InstanceID
  }

type Period =
  { startTime :: Date
  , duration  :: Int
  , endTime   :: Date 
  }

instance showEvent :: (EventCategory a, EventID b) => Show (Event a b) where
  show = JSON.stringify <<< foreignEvent

derive instance eqEvent :: (EventCategory a, EventID b) => Eq (Event a b)

eventTypes :: Array EventType
eventTypes = [ Success, Failure ]

{-- todo: see https://github.com/markfarrell/3tier/issues/27 --}

foreignEvent :: forall a b. EventCategory a => EventID b => Event a b -> Foreign
foreignEvent (Event x) = unsafeCoerce $
  { eventCategory : show x.eventCategory
  , eventType     : show x.eventType
  , eventID       : show x.eventID
  , sourceID      : show x.sourceID
  , sessionID     : show x.sessionID
  , destinationID : show x.destinationID
  , logID         : show x.logID
  , schemaID      : show x.schemaID
  , featureID     : show x.featureID
  , instanceID    : show x.instanceID
  , startTime     : show x.startTime
  , duration      : show x.duration
  , endTime       : show x.endTime
  }
