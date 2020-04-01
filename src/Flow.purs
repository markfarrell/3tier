module Flow
  ( Record (..)
  , flow
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

import Parser as Parser
import IPv4 (IPv4)

data Record = Record
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

{-- Parses a valid SiLk flow record based on the parsers defined for its fields, or fails otherwise. --}
flow :: Parser String Record
flow = do
  sourceIPv4      <- Parser.ipv4
  _               <- comma
  destinationIPv4 <- Parser.ipv4
  _               <- comma
  sourcePort      <- Parser.port
  _               <- comma
  destinationPort <- Parser.port
  _               <- comma
  protocol        <- Parser.octet
  _               <- comma
  packets         <- Parser.positiveInteger
  _               <- comma
  bytes           <- Parser.positiveInteger
  _               <- comma
  flags'          <- flags
  _               <- comma
  startTime           <- Parser.timestamp
  _               <- comma
  duration'       <- Parser.positiveFloat
  _               <- comma
  endTime           <- Parser.timestamp
  _               <- eof
  pure $ Record
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

uri :: Record -> String
uri (Record record) = intercalate delimiter' $
  [ show record.sourceIPv4
  , show record.destinationIPv4
  , show record.sourcePort
  , show record.destinationPort
  , show record.protocol
  , show record.packets
  , show record.bytes
  , record.flags
  , show record.startTime
  , show record.duration
  , show record.endTime
  ]
  where delimiter' = singleton delimiter
