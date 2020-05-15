module Text.Parsing.Event
  ( event
  ) where

import Prelude

import Foreign (Foreign)

import Text.Parsing.Parser (Parser)

import Text.Parsing.Common (json, property, date, nonnegativeInteger, array, uuid, readIndex)

import Data.Event (Event(..))
import Data.Event as Event

eventType :: Parser String Event.EventType
eventType = array $ Event.eventTypes

time :: Foreign -> Parser String Event.EventTime
time = \x -> do
  startTime <- property "startTime" x $ date
  duration  <- property "duration"  x $ nonnegativeInteger
  endTime   <- property "endTime"   x $ date
  pure $ Event.EventTime $
    { startTime : startTime
    , duration  : duration
    , endTime   : endTime
    }

event :: forall a b. Parser String a -> Parser String b -> Parser String (Event a b)
event eventCategory eventID = do
  x              <- json
  eventCategory' <- property  "eventCategory" x $ eventCategory
  eventType'     <- property  "eventType"     x $ eventType
  eventID'       <- property  "eventID"       x $ eventID
  eventTime      <- readIndex "eventTime"     x >>= time 
  sessionID      <- property  "sessionID"     x $ uuid
  featureID      <- property  "featureID"     x $ uuid
  instanceID     <- property  "instanceID"    x $ uuid
  sourceID       <- property  "sourceID"      x $ uuid
  destinationID  <- property  "destinationID" x $ uuid
  pure $ Event $
    { eventCategory : eventCategory'
    , eventType     : eventType'
    , eventID       : eventID'
    , eventTime     : eventTime
    , sessionID     : sessionID
    , featureID     : featureID
    , instanceID    : instanceID
    , sourceID      : sourceID
    , destinationID : destinationID
    }
