module Text.Parsing.Alert
  ( event
  ) where

import Prelude

import Text.Parsing.Parser (Parser)

import Data.Alert as Alert
import Text.Parsing.Common (json, property, showable, readIndex, uuid)
import Text.Parsing.Event (source, time) as Event

event :: Parser String Alert.Event
event = do
  x              <- json
  eventCategory  <- property  "eventCategory" x $ showable Alert.eventCategories
  eventType      <- property  "eventType"     x $ showable Alert.eventTypes
  eventID        <- property  "eventID"       x $ showable Alert.eventIDs
  eventTime      <- readIndex "eventTime"    x >>= Event.time 
  eventSource    <- property  "eventSource"  x $ Event.source
  eventURI'      <- property  "eventURI"     x $ uuid
  pure $ Alert.Event $
    { eventCategory : eventCategory
    , eventType     : eventType
    , eventID       : eventID
    , eventTime     : eventTime
    , eventSource   : eventSource
    , eventURI      : eventURI'
    }
