module Effect.Report
  ( random
  ) where

import Prelude

import Data.Array as Array

import Data.Int as Int
import Data.Maybe (Maybe(..))

import Effect (Effect)

import FFI.Math as Math

import Data.Report as Report 

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

eventCategory :: Effect Report.EventCategory
eventCategory = array default $ Report.eventCategories
  where default = Report.Source

eventID :: Effect Report.EventID
eventID = array default $ Report.eventIDs
  where default = Report.Anomalous

eventType :: Effect Report.EventType
eventType = array Report.Success $ [Report.Success, Report.Failure]

random :: Effect Report.Event
random = do
  eventCategory' <- eventCategory
  eventID'       <- eventID
  eventType'     <- eventType
  pure $ Report.Event $
    { eventCategory : eventCategory'
    , eventID       : eventID'
    , eventType     : eventType'
    , min           : 0
    , max           : 0
    , sum           : 0
    , total         : 0
    , average       : 0
    , variance      : 0
    }
