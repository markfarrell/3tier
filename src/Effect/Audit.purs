module Effect.Audit
  ( random
  ) where

import Prelude

import Data.Array as Array

import Data.Int as Int
import Data.Maybe (Maybe(..))

import Effect (Effect)
import Effect.Date (random) as Date

import FFI.Date (epoch, getTime) as Date
import FFI.Math as Math
import FFI.UUID as UUID

import Data.Audit as Audit 

import Data.Event (Event(..))
import Data.Event as Event

import Effect.Array (random) as Array

random :: Effect Audit.Event
random = do
  eventCategory' <- Array.random Audit.Forward $ Audit.eventCategories
  eventID'       <- Array.random Audit.Alert   $ Audit.eventIDs
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
