module Effect.Windows
  ( random
  ) where

import Prelude

import Effect (Effect)
import Effect.Date (random) as Date

import FFI.Date (epoch, getTime) as Date
import FFI.Math as Math
import FFI.UUID as UUID

import Data.Windows as Windows 

import Data.Event (Event(..))
import Data.Event as Event

import Effect.Array (random) as Array

random :: Effect Windows.Event
random = do
  eventCategory' <- Array.random Windows.AccountLogon $ Windows.eventCategories
  eventID'       <- Array.random 0                    $ Windows.eventIDs' eventCategory'
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
