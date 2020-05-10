module Text.Parsing.Event
  ( source
  , time
  , event
  ) where

import Prelude

import Foreign (Foreign)

import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.String (char)
import Text.Parsing.Parser.Combinators (choice)

import FFI.UUID (UUID)

import Text.Parsing.Common (json, property, date, nonnegativeInteger, array, uuid, readIndex)

import Data.Event (Event(..))
import Data.Event as Event

eventType :: Parser String Event.EventType
eventType = array $ Event.eventTypes

eventURI :: Parser String Event.EventURI
eventURI = uuid

source :: Parser String Event.EventSource
source = uuid

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

event :: forall a b. Show a => Show b => Array a -> Array b -> Parser String (Event a b)
event eventCategories eventIDs = do
  x              <- json
  eventCategory  <- property  "eventCategory" x $ array eventCategories
  eventType'     <- property  "eventType"     x $ eventType
  eventID        <- property  "eventID"       x $ array eventIDs
  eventTime      <- readIndex "eventTime"     x >>= time 
  eventSource    <- property  "eventSource"   x $ source
  eventURI'      <- property  "eventURI"      x $ eventURI
  pure $ Event $
    { eventCategory : eventCategory
    , eventType     : eventType'
    , eventID       : eventID
    , eventTime     : eventTime
    , eventSource   : eventSource
    , eventURI      : eventURI'
    }
