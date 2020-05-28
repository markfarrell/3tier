module Effect.Event
  ( random
  ) where

import Prelude

import Effect (Effect)
import Effect.Date (random) as Date

import FFI.Date (epoch, getTime) as Date
import FFI.Math as Math
import FFI.UUID as UUID

import Data.Event (Event(..), class EventCategory, class EventType, class EventID)
import Data.Event as E

import Data.EventType (EventType(..))

import Effect.Array (random) as Array

random :: forall a b c. EventCategory a => EventType b => EventID c => Effect a -> Effect b -> Effect c -> Effect (Event a b c)
random f g h = do
  eventCategory  <- f
  eventID        <- h
  eventType      <- g
  startTime      <- pure $ Date.epoch
  endTime        <- Date.random
  duration       <- pure $ Math.floor ((Date.getTime endTime) - (Date.getTime startTime))
  sourceID       <- UUID.uuidv4
  sessionID      <- UUID.uuidv4
  destinationID  <- UUID.uuidv4
  logID          <- UUID.uuidv4
  schemaID       <- UUID.uuidv4
  featureID      <- UUID.uuidv4
  instanceID     <- UUID.uuidv4
  pure $ Event $
    { eventCategory
    , eventType
    , eventID
    , sourceID
    , sessionID
    , destinationID
    , logID
    , schemaID
    , featureID
    , instanceID
    , startTime
    , duration
    , endTime
    }
