module Effect.Alert
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

import Data.Event as Event

import Data.Alert as Alert 

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

eventCategory :: Effect Alert.EventCategory
eventCategory = array default $ Alert.eventCategories
  where default = Alert.Source

eventID :: Effect Alert.EventID
eventID = array default $ Alert.eventIDs
  where default = Alert.Audit

eventType :: Effect Alert.EventType
eventType = array default $ [Alert.Success, Alert.Failure]
  where default = Alert.Success

random :: Effect Alert.Event
random = do
  eventCategory' <- eventCategory
  eventID'       <- eventID
  eventType'     <- eventType
  startTime      <- pure $ Date.epoch
  endTime        <- Date.random
  duration       <- pure $ Math.floor ((Date.getTime endTime) - (Date.getTime startTime))
  eventTime      <- pure $ Event.EventTime { startTime : startTime, duration : duration, endTime : endTime }
  eventSource'   <- UUID.uuidv4
  eventURI'      <- UUID.uuidv4
  pure $ Alert.Event $
    { eventCategory : eventCategory'
    , eventID       : eventID'
    , eventType     : eventType'
    , eventSource   : eventSource'
    , eventURI      : eventURI'
    , eventTime     : eventTime
    }
