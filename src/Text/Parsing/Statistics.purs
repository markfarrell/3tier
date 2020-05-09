module Text.Parsing.Statistics
  ( event
  ) where

import Prelude

import Foreign (Foreign)

import Text.Parsing.Parser (Parser)

import Data.Statistics as Statistics
import Text.Parsing.Common (json, property, nonnegativeInteger, array, readIndex)
import Text.Parsing.Event (source, time) as Event

eventURI :: Foreign -> Parser String Statistics.EventURI
eventURI = \x -> do
  min            <- property "min"      x $ nonnegativeInteger
  max            <- property "max"      x $ nonnegativeInteger
  sum            <- property "sum"      x $ nonnegativeInteger
  total          <- property "total"    x $ nonnegativeInteger
  average        <- property "average"  x $ nonnegativeInteger
  variance       <- property "variance" x $ nonnegativeInteger
  pure $ Statistics.EventURI $
    { min      : min
    , max      : max
    , sum      : sum
    , total    : total
    , average  : average
    , variance : variance
    }

event :: Parser String Statistics.Event
event = do
  x              <- json
  eventCategory  <- property "eventCategory" x $ array Statistics.eventCategories
  eventType      <- property "eventType"     x $ array Statistics.eventTypes
  eventID        <- property "eventID"       x $ array Statistics.eventIDs
  eventTime      <- readIndex "eventTime"    x >>= Event.time 
  eventSource    <- property  "eventSource"  x $ Event.source
  eventURI'      <- readIndex "eventURI"     x >>= eventURI
  pure $ Statistics.Event $
    { eventCategory : eventCategory
    , eventType     : eventType
    , eventID       : eventID
    , eventTime     : eventTime
    , eventSource   : eventSource
    , eventURI      : eventURI'
    }
