module Text.Parsing.Statistics
  ( event
  ) where

import Prelude

import Text.Parsing.Parser (Parser)

import Data.Statistics as Statistics
import Text.Parsing.Common (json, property, nonnegativeInteger, showable, readIndex)
import Text.Parsing.Event (time) as Event

event :: Parser String Statistics.Event
event = do
  x              <- json
  eventCategory' <- property "eventCategory" x $ showable Statistics.eventCategories
  eventType'     <- property "eventType"     x $ showable Statistics.eventTypes
  eventID'       <- property "eventID"       x $ showable Statistics.eventIDs
  eventTime'     <- readIndex "time" x >>= Event.time 
  min            <- property "min"           x $ nonnegativeInteger
  max            <- property "max"           x $ nonnegativeInteger
  sum            <- property "sum"           x $ nonnegativeInteger
  total          <- property "total"         x $ nonnegativeInteger
  average        <- property "average"       x $ nonnegativeInteger
  variance       <- property "variance"      x $ nonnegativeInteger
  pure $ Statistics.Event $
    { eventCategory : eventCategory'
    , eventType     : eventType'
    , eventID       : eventID'
    , eventTime     : eventTime'
    , min           : min
    , max           : max
    , sum           : sum
    , total         : total
    , average       : average
    , variance      : variance
    }
