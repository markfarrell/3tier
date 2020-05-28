module Text.Parsing.Event
  ( event
  ) where

import Prelude

import Text.Parsing.Parser (Parser)

import Text.Parsing.Common (json, property, date, nonnegativeInteger, uuid)

import Data.Event (Event(..), class EventCategory, class EventType, class EventID)

event :: forall a b c. EventCategory a => EventType b => EventID c => Parser String a -> Parser String b -> Parser String c -> Parser String (Event a b c)
event f g h = do
  x              <- json
  eventCategory  <- property  "eventCategory" x $ f
  eventType      <- property  "eventType"     x $ g
  eventID        <- property  "eventID"       x $ h
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
