module Text.Parsing.Windows
  ( event 
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Text.Parsing.Parser (Parser, fail)

import Data.Windows as Windows
import Text.Parsing.Common (date, json, ipv4, port, positiveInteger, property, showable)

event :: Parser String Windows.Event
event = do
  x              <- json
  eventCategory' <- property "eventCategory" x $ showable Windows.eventCategories
  eventID'       <- property "eventID"       x $ eventID eventCategory' 
  eventType'     <- property "eventType"     x $ showable Windows.eventTypes
  startTime'     <- property "startTime"     x $ date
  duration'      <- property "duration"      x $ positiveInteger
  endTime'       <- property "endTime"       x $ date
  sIP'           <- property "sIP"           x $ ipv4
  sPort'         <- property "sPort"         x $ port
  case eventID' of
    (Tuple eventCategory'' eventID'') -> do
      eventURI'      <- pure $ Windows.Security
                          { eventID            : eventID''
                          , machineName        : unit
                          , entryData          : unit
                          , category           : unit
                          , categoryNumber     : unit
                          , entryType          : eventType'
                          , message            : unit
                          , source             : unit 
                          , replacementStrings : unit 
                          , instanceID         : unit 
                          , timeGenerated      : startTime' 
                          , timeWritten        : endTime'
                          , site               : unit 
                          , container          : unit 
                          }
      case eventCategory' == eventCategory'' of
        true  -> pure $ Windows.Event
          { eventCategory : eventCategory''
          , eventID       : eventID''
          , eventType     : eventType'
          , eventURI      : eventURI'
          , startTime     : startTime'
          , duration      : duration'
          , endTime       : endTime'
          , sIP           : sIP'
          , sPort         : sPort'
          }
        false -> fail "Invalid (eventCategory, eventID)."

eventID :: Windows.EventCategory -> Parser String (Tuple Windows.EventCategory Windows.EventID)
eventID = \eventCategory' -> do
  result <- positiveInteger
  case Array.elemIndex result (Windows.eventIDs eventCategory') of
    (Just _)  -> pure (Tuple eventCategory' result)
    (Nothing) -> fail $ "Invalid eventCategory/eventID (" <> show eventCategory' <> "," <> show result <> ")"
