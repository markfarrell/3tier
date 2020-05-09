module Text.Parsing.Windows
  ( event
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Text.Parsing.Parser (Parser, fail)

import Data.Windows as Windows

import Text.Parsing.Common as Common
import Text.Parsing.Event (source, time) as Event

event :: Parser String Windows.Event
event = do
  x              <- Common.json
  eventCategory' <- Common.property "eventCategory" x $ Common.showable Windows.eventCategories
  eventID'       <- Common.property "eventID"       x $ eventID eventCategory'
  eventType'     <- Common.property "eventType"     x $ Common.showable Windows.eventTypes
  eventURI'      <- Common.property "eventURI"      x $ Common.uuid
  eventTime'     <- Common.readIndex "eventTime"    x >>= Event.time
  eventSource'   <- Common.property "eventSource"   x $ Event.source
  case eventID' of
    (Tuple eventCategory'' eventID'') -> do
      case eventCategory' == eventCategory'' of
        true  -> pure $ Windows.Event
          { eventCategory : eventCategory''
          , eventID       : eventID''
          , eventType     : eventType'
          , eventURI      : eventURI'
          , eventTime     : eventTime'
          , eventSource   : eventSource'
          }
        false -> fail "Invalid (eventCategory, eventID)."

eventID :: Windows.EventCategory -> Parser String (Tuple Windows.EventCategory Windows.EventID)
eventID = \eventCategory' -> do
  result <- Common.nonnegativeInteger
  case Array.elemIndex result (Windows.eventIDs eventCategory') of
    (Just _)  -> pure (Tuple eventCategory' result)
    (Nothing) -> fail $ "Invalid eventCategory/eventID (" <> show eventCategory' <> "," <> show result <> ")"
