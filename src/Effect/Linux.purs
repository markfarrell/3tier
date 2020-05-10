module Effect.Linux
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

import Data.IPv4 (IPv4(..))
import Data.Linux as Linux 
import Data.Event as Event

range :: Int -> Int -> Effect Int
range min max = do
  w <- Math.random
  x <- pure $ Int.toNumber min
  y <- pure $ Int.toNumber max
  z <- pure $ w * (y - x) + x 
  pure $ Math.floor z

index :: forall a. Array a -> Effect Int
index w = range 0 ((Array.length w) - 1)

array' :: forall a. Array a -> Effect (Maybe a)
array' w = do
  x <- index w
  y <- pure $ Array.index w x
  pure y

array ::forall a. a -> Array a -> Effect a
array w x = do
  y <- array' x
  case y of
    (Just z)  -> pure z
    (Nothing) -> pure w

eventCategory :: Effect Linux.EventCategory
eventCategory = array default $ Linux.eventCategories
  where default = Linux.DaemonStart

eventID :: Effect Linux.EventID
eventID = range 0 2147483647

eventType :: Effect Linux.EventType
eventType = array Linux.Success $ Linux.eventTypes

random :: Effect Linux.Event
random = do
  eventCategory' <- eventCategory
  eventID'       <- eventID
  eventType'     <- eventType
  startTime      <- pure $ Date.epoch
  endTime        <- Date.random
  duration       <- pure $ Math.floor ((Date.getTime endTime) - (Date.getTime startTime))
  eventURI       <- UUID.uuidv4
  eventSource    <- UUID.uuidv4
  eventTime      <- pure $ Event.EventTime { startTime : startTime, duration : duration, endTime : endTime }
  pure $ Linux.Event $
    { eventCategory : eventCategory'
    , eventID       : eventID'
    , eventType     : eventType'
    , eventURI      : eventURI
    , eventSource   : eventSource
    , eventTime     : eventTime
    }
