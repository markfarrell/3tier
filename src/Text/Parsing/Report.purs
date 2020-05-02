module Text.Parsing.Report
  ( event
  ) where

import Prelude

import Text.Parsing.Parser (Parser)

import Data.Report as Report
import Text.Parsing.Common (json, property, nonnegativeInteger, showable)

event :: Parser String Report.Event
event = do
  x              <- json
  eventCategory' <- property "eventCategory" x $ showable Report.eventCategories
  eventType'     <- property "eventType"     x $ showable Report.eventTypes
  eventID'       <- property "eventID"       x $ showable Report.eventIDs
  min            <- property "min"           x $ nonnegativeInteger
  max            <- property "max"           x $ nonnegativeInteger
  sum            <- property "sum"           x $ nonnegativeInteger
  total          <- property "total"         x $ nonnegativeInteger
  average        <- property "average"       x $ nonnegativeInteger
  variance       <- property "variance"      x $ nonnegativeInteger
  pure $ Report.Event $
    { eventCategory : eventCategory'
    , eventType     : eventType'
    , eventID       : eventID'
    , min           : min
    , max           : max
    , sum           : sum
    , total         : total
    , average       : average
    , variance      : variance
    }
