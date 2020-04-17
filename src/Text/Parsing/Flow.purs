module Text.Parsing.Flow
  ( event
  ) where

import Prelude

import Text.Parsing.Parser (Parser)

import Text.Parsing.Common (date, flags, json, port, ipv4, octet, positiveInteger, property)

import Data.Flow as Flow

event :: Parser String Flow.Event
event = do
  x         <- json
  sIP       <- property "sIP"       x  $ ipv4
  dIP       <- property "dIP"       x  $ ipv4
  sPort     <- property "sPort"     x  $ port
  dPort     <- property "dPort"     x  $ port
  protocol  <- property "protocol"  x  $ octet
  packets   <- property "packets"   x  $ positiveInteger
  bytes     <- property "bytes"     x  $ positiveInteger
  flags'    <- property "flags"     x  $ flags
  startTime <- property "startTime" x  $ date
  duration' <- property "duration"  x  $ positiveInteger
  endTime   <- property "endTIme"   x  $ date
  pure $ Flow.Event
    { sIP       : sIP
    , dIP       : dIP
    , sPort     : sPort
    , dPort     : dPort
    , protocol  : protocol
    , packets   : packets
    , bytes     : bytes
    , flags     : flags'
    , startTime : startTime
    , duration  : duration'
    , endTime   : endTime
    }
