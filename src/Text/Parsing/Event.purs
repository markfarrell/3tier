module Text.Parsing.Event
  ( eventSource
  , eventTime
  ) where

import Prelude

import Text.Parsing.Parser (Parser)

import Text.Parsing.Common (ipv4, port, json, property, date, nonnegativeInteger)

import Data.Event as Event

eventSource :: Parser String Event.Entity
eventSource = do
  x    <- json
  ip   <- property "ip"   x  $ ipv4
  port <- property "port" x  $ port
  pure $ Event.Host $
    { ip   : ip
    , port : port
    }

eventTime :: Parser String Event.Time
eventTime = do
  x         <- json
  startTime <- property "startTime" x $ date
  duration  <- property "duration"  x $ nonnegativeInteger
  endTime   <- property "endTime"   x $ date
  pure $ Event.Time $
    { startTime : startTime
    , duration  : duration
    , endTime   : endTime
    }
