module Text.Parsing.Event
  ( source
  , time
  ) where

import Prelude

import Foreign (Foreign)

import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.String (char)
import Text.Parsing.Parser.Combinators (choice)

import FFI.UUID (UUID)

import Text.Parsing.Common (ipv4, port, property, date, nonnegativeInteger, array, uuid)

import Data.Event as Event

source :: Parser String UUID
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
