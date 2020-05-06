module Text.Parsing.Event
  ( source
  , time
  ) where

import Prelude

import Foreign (Foreign)

import Text.Parsing.Parser (Parser)

import Text.Parsing.Common (ipv4, port, json, property, date, nonnegativeInteger)

import Data.Event as Event

source :: Parser String Event.Entity
source = do
  x    <- json
  ip   <- property "ip"   x  $ ipv4
  port <- property "port" x  $ port
  pure $ Event.Host $
    { ip   : ip
    , port : port
    }

time :: Foreign -> Parser String Event.Time
time = \x -> do
  startTime <- property "startTime" x $ date
  duration  <- property "duration"  x $ nonnegativeInteger
  endTime   <- property "endTime"   x $ date
  pure $ Event.Time $
    { startTime : startTime
    , duration  : duration
    , endTime   : endTime
    }
