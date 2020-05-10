module Effect.Traffic
  ( random
  ) where

import Prelude

import Effect (Effect)
import Effect.Date (random) as Date

import FFI.Date (epoch, getTime) as Date
import FFI.Math as Math
import FFI.UUID as UUID

import Data.Traffic as Traffic 

import Data.Event (Event(..))
import Data.Event as Event

import Effect.Array (random) as Array

import Unsafe.Coerce (unsafeCoerce)

random :: Effect Traffic.Event
random = do
  eventCategory' <- Array.random Traffic.In       $ Traffic.eventCategories
  eventID'       <- Array.random (unsafeCoerce 0) $ Traffic.eventIDs
  eventType'     <- Array.random Event.Success $ Event.eventTypes
  startTime      <- pure $ Date.epoch
  endTime        <- Date.random
  duration       <- pure $ Math.floor ((Date.getTime endTime) - (Date.getTime startTime))
  eventTime      <- pure $ Event.EventTime { startTime : startTime, duration : duration, endTime : endTime }
  eventSource'   <- UUID.uuidv4
  eventURI'      <- UUID.uuidv4
  pure $ Event $
    { eventCategory : eventCategory'
    , eventID       : eventID'
    , eventType     : eventType'
    , eventSource   : eventSource'
    , eventURI      : eventURI'
    , eventTime     : eventTime
    }
