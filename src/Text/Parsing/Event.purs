module Text.Parsing.Event
  ( event
  ) where

import Prelude

import Foreign (Foreign)

import Text.Parsing.Parser (Parser)

import Text.Parsing.Common (json, property, date, nonnegativeInteger, array, uuid, readIndex)

import Data.Event (Event(..), class EventCategory, class EventID)
import Data.Event as Event

event :: forall a b. EventCategory a => EventID b => Parser String a -> Parser String b -> Parser String (Event a b)
event eventCategory eventID = do
  x              <- json
  eventCategory' <- property  "eventCategory" x $ eventCategory
  eventType'     <- property  "eventType"     x $ array Event.eventTypes
  eventID'       <- property  "eventID"       x $ eventID
  sessionID      <- property  "sessionID"     x $ uuid
  featureID      <- property  "featureID"     x $ uuid
  instanceID     <- property  "instanceID"    x $ uuid
  sourceID       <- property  "sourceID"      x $ uuid
  destinationID  <- property  "destinationID" x $ uuid
  startTime      <- property  "startTime"     x $ date
  duration       <- property  "duration"      x $ nonnegativeInteger
  endTime        <- property  "endTime"       x $ date
  pure $ Event $
    { eventCategory : eventCategory'
    , eventType     : eventType'
    , eventID       : eventID'
    , sessionID     : sessionID
    , featureID     : featureID
    , instanceID    : instanceID
    , sourceID      : sourceID
    , destinationID : destinationID
    , startTime     : startTime
    , duration      : duration
    , endTime       : endTime
    }
