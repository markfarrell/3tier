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
  ) where

import Prelude

import Foreign (Foreign)

import Unsafe.Coerce (unsafeCoerce)

import FFI.Date (Date)
import FFI.JSON (stringify) as JSON
import FFI.UUID (UUID)

import Data.Event.Class (class EventCategory, class EventID, class EventType, eventCategories, eventTypes, eventIDs)

type SourceID      = UUID
type SessionID     = UUID
type DestinationID = UUID
type LogID         = UUID
type SchemaID      = UUID

type FeatureID     = UUID
type InstanceID    = UUID 

data Event a b c = Event (EventCategory a => EventType b => EventID c =>
  { eventCategory :: a
  , eventType     :: b
  , eventID       :: c
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

type Feature a b c = (EventCategory a => EventType b => EventID c =>
  { eventCategory :: a 
  , eventType     :: b
  , eventID       :: c
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

instance showEvent :: (EventCategory a, EventType b, EventID c) => Show (Event a b c) where
  show = JSON.stringify <<< marshall

derive instance eqEvent :: (EventCategory a, EventType b, EventID c) => Eq (Event a b c)

{-- todo: see https://github.com/markfarrell/3tier/issues/27 --}

marshall :: forall a b c. EventCategory a => EventType b => EventID c => Event a b c -> Foreign
marshall (Event x) = unsafeCoerce $
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
