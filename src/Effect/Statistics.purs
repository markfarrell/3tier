module Effect.Statistics
  ( random
  ) where

import Prelude

import Effect (Effect)

import FFI.Date   as Date
import FFI.Number as Number
import FFI.UUID   as UUID

import Data.Event as Event
import Data.Statistics as Statistics 

import Effect.Array (random) as Array
import Effect.Range (random) as Range

random :: Effect Statistics.Event
random = do
  eventCategory' <- Array.random Statistics.Source $ Statistics.eventCategories
  eventID'       <- Array.random Statistics.Audit  $ Statistics.eventIDs
  eventType'     <- Array.random Event.Success     $ Event.eventTypes
  eventTime'     <- pure $ Event.EventTime { startTime : Date.epoch, duration : 0, endTime : Date.epoch }
  eventSource'   <- UUID.uuidv4
  min            <- Range.random 0 Number.maxSafeInteger
  max            <- Range.random 0 Number.maxSafeInteger
  sum            <- Range.random 0 Number.maxSafeInteger
  total          <- Range.random 0 Number.maxSafeInteger
  average        <- Range.random 0 Number.maxSafeInteger
  variance       <- Range.random 0 Number.maxSafeInteger
  eventURI'      <- pure $ Statistics.EventURI $
    { min           : min
    , max           : max
    , sum           : sum
    , total         : total
    , average       : average
    , variance      : variance
    }
  pure $ Statistics.Event $
    { eventCategory : eventCategory'
    , eventID       : eventID'
    , eventType     : eventType'
    , eventTime     : eventTime'
    , eventSource   : eventSource'
    , eventURI      : eventURI'
    }
