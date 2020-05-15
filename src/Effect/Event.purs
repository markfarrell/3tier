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
random eventCategory eventID = do
  eventCategory' <- eventCategory
  eventID'       <- eventID
  eventType'     <- Array.random Event.Success $ Event.eventTypes
  startTime      <- pure $ Date.epoch
  endTime        <- Date.random
  duration       <- pure $ Math.floor ((Date.getTime endTime) - (Date.getTime startTime))
  eventTime      <- pure $ Event.EventTime { startTime : startTime, duration : duration, endTime : endTime }
  sessionID      <- UUID.uuidv4
  featureID      <- UUID.uuidv4
  instanceID     <- UUID.uuidv4
  sourceID       <- UUID.uuidv4
  destinationID  <- UUID.uuidv4
  pure $ Event $
    { eventCategory : eventCategory'
    , eventID       : eventID'
    , eventType     : eventType'
    , sessionID     : sessionID
    , featureID     : featureID
    , instanceID    : instanceID
    , sourceID      : sourceID
    , destinationID : destinationID
    , eventTime     : eventTime
    }
