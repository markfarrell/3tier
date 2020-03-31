module Flow
  ( Entry (..)
  , flow
  , uri
  ) where

import Prelude

import Effect.Exception (Error)
import Effect.Exception (error) as Exception

import Data.List as List

import Data.Either (Either(..))

import Data.Identity (Identity)
import Data.Newtype (unwrap)

import Data.Traversable(foldMap)
import Data.String.CodeUnits (singleton)

import Text.Parsing.Parser (Parser, fail, runParser)
import Text.Parsing.Parser.String (char, eof, string)
import Text.Parsing.Parser.Combinators (choice)

import Arrays as Arrays
import Parser as Parser

import Date (Date)
import IPv4 (IPv4)

newtype Entry = Entry
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
flow :: Parser String Entry
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
  pure $ Entry
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

uri :: Entry -> String
uri (Entry entry) = Arrays.join delimiter' $
  [ show entry.sourceIPv4
  , show entry.destinationIPv4
  , show entry.sourcePort
  , show entry.destinationPort
  , show entry.protocol
  , show entry.packets
  , show entry.bytes
  , entry.flags
  , show entry.startTime
  , show entry.duration
  , show entry.endTime
  ]
  where delimiter' = singleton delimiter
