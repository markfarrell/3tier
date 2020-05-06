module Text.Parsing.Event
  ( source
  , time
  ) where

import Prelude

import Foreign (Foreign)

import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.String (char)
import Text.Parsing.Parser.Combinators (choice)

import Text.Parsing.Common (ipv4, port, property, date, nonnegativeInteger, showable)

import Data.Event as Event

host :: Parser String Event.Source
host = do
  ip   <- ipv4
  _    <- char ':'
  port <- port
  pure $ Event.Host $
    { ip   : ip
    , port : port
    }

tier :: Parser String Event.Source
tier = showable [ Event.Tier1, Event.Tier2, Event.Tier3]

source :: Parser String Event.Source
source = choice [ host, tier ]

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
