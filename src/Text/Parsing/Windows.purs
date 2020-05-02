module Text.Parsing.Windows
  ( event
  , eventURI
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Text.Parsing.Parser (Parser, fail)

import Data.Windows as Windows

import Text.Parsing.Common (date, json, nonnegativeInteger, property, propertyNot, showable)
import Text.Parsing.Event (eventSource, eventTime) as Event
import Text.Parsing.Risk (injection) as Risk

eventURI :: Parser String Windows.EventURI
eventURI = do
  x                  <- json
  eventID'           <- property    "eventID"            x $ showable Windows.eventIDs' 
  machineName        <- propertyNot "machineName"        x $ Risk.injection
  entryNumber        <- property    "entryNumber"        x $ nonnegativeInteger
  entryData          <- propertyNot "entryData"          x $ Risk.injection
  category           <- propertyNot "category"           x $ Risk.injection
  categoryNumber     <- property    "categoryNumber"     x $ nonnegativeInteger
  entryType          <- property    "entryType"          x $ showable Windows.eventTypes
  message            <- propertyNot "message"            x $ Risk.injection
  source             <- propertyNot "source"             x $ Risk.injection
  replacementStrings <- propertyNot "replacementStrings" x $ Risk.injection
  instanceID         <- propertyNot "instanceID"         x $ Risk.injection
  timeGenerated      <- property    "timeGenerated"      x $ date
  timeWritten        <- property    "timeWritten"        x $ date
  site               <- propertyNot "site"               x $ Risk.injection
  container          <- propertyNot "container"          x $ Risk.injection
  pure $ Windows.Security $
    { eventID            : eventID'
    , machineName        : machineName
    , entryNumber        : entryNumber
    , entryData          : entryData
    , category           : category
    , categoryNumber     : categoryNumber
    , entryType          : entryType
    , message            : message
    , source             : source
    , replacementStrings : replacementStrings
    , instanceID         : instanceID
    , timeGenerated      : timeGenerated
    , timeWritten        : timeWritten
    , site               : site
    , container          : container
    }

event :: Parser String Windows.Event
event = do
  x              <- json
  eventCategory' <- property "eventCategory" x $ showable Windows.eventCategories
  eventID'       <- property "eventID"       x $ eventID eventCategory'
  eventType'     <- property "eventType"     x $ showable Windows.eventTypes
  eventURI'      <- property "eventURI"      x $ eventURI
  eventTime'     <- property "eventTime"     x $ Event.eventTime
  eventSource'   <- property "eventSource"   x $ Event.eventSource
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
  result <- nonnegativeInteger
  case Array.elemIndex result (Windows.eventIDs eventCategory') of
    (Just _)  -> pure (Tuple eventCategory' result)
    (Nothing) -> fail $ "Invalid eventCategory/eventID (" <> show eventCategory' <> "," <> show result <> ")"
