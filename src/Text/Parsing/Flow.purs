module Text.Parsing.Flow
  ( event
  ) where

import Prelude

import Text.Parsing.Parser (Parser)

import Text.Parsing.Common (date, json, port, ipv4, octet, nonnegativeInteger, property)
import Text.Parsing.TCP.Flag (flags) as Flag

import Data.Flow as Flow

event :: Parser String Flow.Event
event = do
  x         <- json
  sIP       <- property "sIP"       x  $ ipv4
  dIP       <- property "dIP"       x  $ ipv4
  sPort     <- property "sPort"     x  $ port
  dPort     <- property "dPort"     x  $ port
  protocol  <- property "protocol"  x  $ octet
  packets   <- property "packets"   x  $ nonnegativeInteger
  bytes     <- property "bytes"     x  $ nonnegativeInteger
  flags     <- property "flags"     x  $ Flag.flags
  startTime <- property "startTime" x  $ date
  duration  <- property "duration"  x  $ nonnegativeInteger
  endTime   <- property "endTime"   x  $ date
  pure $ Flow.Event
    { sIP
    , dIP
    , sPort
    , dPort
    , protocol
    , packets
    , bytes
    , flags
    , startTime
    , duration
    , endTime
    }
