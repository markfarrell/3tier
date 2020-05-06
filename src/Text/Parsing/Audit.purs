module Text.Parsing.Audit
  ( event
  ) where

import Prelude

import Text.Parsing.Parser (Parser)

import Data.Audit as Audit
import Text.Parsing.Common (json, property, showable, readIndex, uuid)
import Text.Parsing.Event (source, time) as Event

event :: Parser String Audit.Event
event = do
  x              <- json
  eventCategory  <- property  "eventCategory" x $ showable Audit.eventCategories
  eventType      <- property  "eventType"     x $ showable Audit.eventTypes
  eventID        <- property  "eventID"       x $ showable Audit.eventIDs
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
