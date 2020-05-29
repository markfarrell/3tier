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

import Foreign.Class (class Marshall, marshall) as F
import Foreign.Coerce (coerce) as F

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
  show = JSON.stringify <<< F.marshall

derive instance eqEvent :: (EventCategory a, EventType b, EventID c) => Eq (Event a b c)

{-- todo: see https://github.com/markfarrell/3tier/issues/27 --}
{-- todo: use `purescript-heterogenous` (?) --}
instance marshallEvent :: (EventCategory a, EventType b, EventID c) => F.Marshall (Event a b c) where
  marshall (Event x) = F.coerce $
    { eventCategory : F.marshall x.eventCategory
    , eventType     : F.marshall x.eventType
    , eventID       : F.marshall x.eventID
    , sourceID      : F.marshall x.sourceID
    , sessionID     : F.marshall x.sessionID
    , destinationID : F.marshall x.destinationID
    , logID         : F.marshall x.logID
    , schemaID      : F.marshall x.schemaID
    , featureID     : F.marshall x.featureID
    , instanceID    : F.marshall x.instanceID
    , startTime     : F.marshall x.startTime
    , duration      : F.marshall x.duration
    , endTime       : F.marshall x.endTime
    }
