module Effect.Flow
  ( random
  ) where

import Prelude

import Effect (Effect)
import Effect.Date (random) as Date

import FFI.Date (epoch, getTime) as Date

import FFI.Math as Math
import FFI.Number as Number

import Data.IPv4 (IPv4(..))
import Data.TCP.Flag (Flag(..))

import Data.Flow as Flow 

import Effect.Port as Port
import Effect.Range as Range

octet :: Effect Int
octet = Range.random 0 255

ipv4 :: Effect IPv4
ipv4 = do
  w <- octet
  x <- octet
  y <- octet
  z <- octet
  pure $ IPv4 w x y z

random :: Effect Flow.Event
random = do
  sIP       <- ipv4
  sPort     <- Port.random
  dIP       <- ipv4
  dPort     <- Port.random
  packets   <- Range.random 0 Number.maxSafeInteger
  bytes     <- Range.random 0 Number.maxSafeInteger
  protocol  <- octet
  flags     <- pure $ [U true, R true, F true,S true, P true, A true]
  startTime <- pure $ Date.epoch
  endTime   <- Date.random
  duration  <- pure $ Math.floor ((Date.getTime endTime) - (Date.getTime startTime))
  pure $ Flow.Event $
    { sIP       : sIP
    , sPort     : sPort
    , dIP       : dIP
    , dPort     : dPort
    , protocol  : protocol
    , packets   : packets
    , bytes     : bytes
    , flags     : flags
    , startTime : startTime
    , duration  : duration
    , endTime   : endTime 
    }
