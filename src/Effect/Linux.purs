module Effect.Linux
  ( random
  ) where

import Prelude

import Effect (Effect)
import Effect.Date (random) as Date

import FFI.Date (epoch, getTime) as Date
import FFI.Math as Math
import FFI.Number as Number
import FFI.UUID as UUID

import Data.Linux as Linux 

import Data.Event (Event(..))
import Data.Event as Event

import Effect.Array (random) as Array
import Effect.Range (random) as Range

random :: Effect Linux.Event
random = do
  eventCategory' <- Array.random Linux.DaemonStart $ Event.eventCategories
  eventID'       <- Linux.EventID <$> Range.random 0 Number.maxSafeInteger
  eventType'     <- Array.random Event.Success     $ Event.eventTypes
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
