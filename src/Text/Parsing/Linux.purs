module Text.Parsing.Linux
  ( event
  ) where

import Prelude

import Text.Parsing.Parser (Parser)

import Data.Linux as Linux

import Text.Parsing.Common as Common
import Text.Parsing.Event (source, time) as Event

event :: Parser String Linux.Event
event = do
  x              <- Common.json
  eventCategory <- Common.property "eventCategory" x $ Common.array Linux.eventCategories
  eventID       <- Common.property "eventID"       x $ Common.nonnegativeInteger
  eventType     <- Common.property "eventType"     x $ Common.array Linux.eventTypes
  eventURI      <- Common.property "eventURI"      x $ Common.uuid
  eventTime     <- Common.readIndex "eventTime"    x >>= Event.time
  eventSource   <- Common.property "eventSource"   x $ Event.source
  pure $ Linux.Event
    { eventCategory : eventCategory
    , eventID       : eventID
    , eventType     : eventType
    , eventURI      : eventURI
    , eventTime     : eventTime
    , eventSource   : eventSource
    }
