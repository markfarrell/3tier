module Effect.Event
  ( random
  ) where

import Prelude

import Effect (Effect)
import Effect.Date (random) as Date

import FFI.Date (epoch, getTime) as Date
import FFI.Math as Math
import FFI.UUID as UUID

import Data.Event (Event(..), class EventCategory, class EventID)
import Data.Event as Event

import Effect.Array (random) as Array

random :: forall a b. EventCategory a => EventID b => Effect a -> Effect b -> Effect (Event a b)
random f g = do
  eventCategory  <- f
  eventID        <- g
  eventType      <- Array.random Event.Success $ Event.eventTypes
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
