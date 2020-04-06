module Effect.Windows
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

import Data.IPv4 (IPv4(..))
import Data.Windows as Windows 

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

eventCategory :: Effect Windows.EventCategory
eventCategory = array default $ Windows.eventCategories
  where default = Windows.Uncategorized

eventID :: Windows.EventCategory -> Effect Windows.EventID
eventID w = array default (Windows.eventIDs w) 
  where default = 5050

eventType :: Effect Windows.EventType
eventType = array Windows.Success $ [Windows.Success, Windows.Failure]

random :: Effect Windows.Event
random = do
  eventCategory' <- eventCategory
  eventID'       <- eventID eventCategory'
  eventType'     <- eventType
  startTime      <- pure $ Date.epoch
  endTime        <- Date.random
  duration       <- pure $ Math.floor ((Date.getTime endTime) - (Date.getTime startTime))
  sIP            <- pure $ IPv4 0 0 0 0
  sPort          <- pure $ 0
  pure $ Windows.Event $
    { eventCategory : eventCategory'
    , eventID       : eventID'
    , eventType     : eventType'
    , startTime     : startTime
    , duration      : duration
    , endTime       : endTime
    , sIP           : sIP
    , sPort         : sPort
    }