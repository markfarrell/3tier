module Flow
  ( Event (..)
  , event
  , uri
  ) where

import Prelude

import Data.List as List
import Data.Foldable (intercalate)

import Data.Traversable(foldMap)
import Data.String.CodeUnits (singleton)

import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.String (char, eof, string)
import Text.Parsing.Parser.Combinators (choice)

import FFI.Date (Date)

import Parser (date, port, ipv4, octet, positiveFloat, positiveInteger)
import IPv4 (IPv4)

data Event = Event
  { sourceIPv4      :: IPv4
  , destinationIPv4 :: IPv4
  , sourcePort      :: Int
  , destinationPort :: Int
  , protocol        :: Int
  , packets         :: Int
  , bytes           :: Int
  , flags           :: String
  , startTime       :: Date
  , duration        :: Number
  , endTime         :: Date
  }

delimiter :: Char
delimiter = ','

flag :: Parser String String
flag = choice (string <$> ["U", "A", "P", "R", "S", "F"])

flags :: Parser String String
flags = do
  elems <- List.many flag
  count <- pure $ List.length elems
  case (count >= 0) && (count <= 6) of
    true  -> pure $ foldMap identity elems
    false -> fail "Invalid number of TCP flags."

{-- Parses a valid SiLk event event based on the parsers defined for its fields, or fails otherwise. --}
event :: Parser String Event
event = do
  sourceIPv4      <- ipv4
  _               <- comma
  destinationIPv4 <- ipv4
  _               <- comma
  sourcePort      <- port
  _               <- comma
  destinationPort <- port
  _               <- comma
  protocol        <- octet
  _               <- comma
  packets         <- positiveInteger
  _               <- comma
  bytes           <- positiveInteger
  _               <- comma
  flags'          <- flags
  _               <- comma
  startTime       <- date
  _               <- comma
  duration'       <- positiveFloat
  _               <- comma
  endTime         <- date
  _               <- eof
  pure $ Event
    { sourceIPv4      : sourceIPv4
    , destinationIPv4 : destinationIPv4
    , sourcePort      : sourcePort
    , destinationPort : destinationPort
    , protocol        : protocol
    , packets         : packets
    , bytes           : bytes
    , flags           : flags'
    , startTime       : startTime
    , duration        : duration'
    , endTime         : endTime
    }
  where comma = char delimiter

uri :: Event -> String
uri (Event event') = intercalate delimiter' $
  [ show event'.sourceIPv4
  , show event'.destinationIPv4
  , show event'.sourcePort
  , show event'.destinationPort
  , show event'.protocol
  , show event'.packets
  , show event'.bytes
  , event'.flags
  , show event'.startTime
  , show event'.duration
  , show event'.endTime
  ]
  where delimiter' = singleton delimiter
