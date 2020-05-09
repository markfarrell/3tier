module Text.Parsing.Audit
  ( event
  ) where

import Prelude

import Text.Parsing.Parser (Parser)

import Data.Audit as Audit
import Text.Parsing.Common (json, property, array, readIndex, uuid)
import Text.Parsing.Event (source, time) as Event

event :: Parser String Audit.Event
event = do
  x              <- json
  eventCategory  <- property  "eventCategory" x $ array Audit.eventCategories
  eventType      <- property  "eventType"     x $ array Audit.eventTypes
  eventID        <- property  "eventID"       x $ array Audit.eventIDs
  eventTime      <- readIndex "eventTime"    x >>= Event.time 
  eventSource    <- property  "eventSource"  x $ Event.source
  eventURI'      <- property  "eventURI"     x $ uuid
  pure $ Audit.Event $
    { eventCategory : eventCategory
    , eventType     : eventType
    , eventID       : eventID
    , eventTime     : eventTime
    , eventSource   : eventSource
    , eventURI      : eventURI'
    }
