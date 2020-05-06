module Effect.Statistics
  ( random
  ) where

import Prelude

import Data.Array as Array

import Data.Int as Int
import Data.Maybe (Maybe(..))

import Effect (Effect)

import FFI.Date as Date
import FFI.Math as Math

import Data.Event as Event
import Data.Statistics as Statistics 

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

eventCategory :: Effect Statistics.EventCategory
eventCategory = array default $ Statistics.eventCategories
  where default = Statistics.Source

eventID :: Effect Statistics.EventID
eventID = array default $ Statistics.eventIDs
  where default = Statistics.Anomalous

eventType :: Effect Statistics.EventType
eventType = array Statistics.Success $ [Statistics.Success, Statistics.Failure]

random :: Effect Statistics.Event
random = do
  eventCategory' <- eventCategory
  eventID'       <- eventID
  eventType'     <- eventType
  eventTime'     <- pure $ Event.Time { startTime : Date.epoch, duration : 0, endTime : Date.epoch }
  pure $ Statistics.Event $
    { eventCategory : eventCategory'
    , eventID       : eventID'
    , eventType     : eventType'
    , eventTime     : eventTime'
    , min           : 0
    , max           : 0
    , sum           : 0
    , total         : 0
    , average       : 0
    , variance      : 0
    }
