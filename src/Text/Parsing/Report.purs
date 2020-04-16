module Text.Parsing.Report
  ( event
  ) where

import Prelude

import Text.Parsing.Parser (Parser)

import Data.Report as Report
import Text.Parsing.Common (json, property, positiveInteger, showable)

event :: Parser String Report.Event
event = do
  x              <- json
  eventCategory' <- property "eventCategory" x $ showable Report.eventCategories
  eventType'     <- property "eventType"     x $ showable Report.eventTypes
  eventID'       <- property "eventID"       x $ showable Report.eventIDs
  min            <- property "min"           x $ positiveInteger
  max            <- property "max"           x $ positiveInteger
  sum            <- property "sum"           x $ positiveInteger
  total          <- property "total"         x $ positiveInteger
  average        <- property "average"       x $ positiveInteger
  variance       <- property "variance"      x $ positiveInteger
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
