module Text.Parsing.Event
  ( event
  ) where

import Prelude

import Text.Parsing.Parser (Parser)

import Text.Parsing.Common (json, property, date, nonnegativeInteger, array, uuid)

import Data.Event (Event(..), class EventCategory, class EventID)
import Data.Event as E
import Data.EventType (EventType)

event :: forall a b. EventCategory a => EventID b => Parser String a -> Parser String b -> Parser String (Event a EventType b)
event v w = do
  x              <- json
  eventCategory  <- property  "eventCategory" x $ v
  eventType      <- property  "eventType"     x $ array E.eventTypes
  eventID        <- property  "eventID"       x $ w
  sourceID       <- property  "sourceID"      x $ uuid
  sessionID      <- property  "sessionID"     x $ uuid
  destinationID  <- property  "destinationID" x $ uuid
  logID          <- property  "logID"         x $ uuid
  schemaID       <- property  "schemaID"      x $ uuid
  featureID      <- property  "featureID"     x $ uuid
  instanceID     <- property  "instanceID"    x $ uuid
  startTime      <- property  "startTime"     x $ date
  duration       <- property  "duration"      x $ nonnegativeInteger
  endTime        <- property  "endTime"       x $ date
  pure $ Event $
    { eventCategory
    , eventType
    , eventID
    , sourceID
    , sessionID
    , destinationID
    , logID
    , schemaID
    , featureID
    , instanceID
    , startTime
    , duration
    , endTime
    }
