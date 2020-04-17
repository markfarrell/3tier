module Effect.Flow
  ( random
  ) where

import Prelude

import Data.Int as Int

import Effect (Effect)
import Effect.Date (random) as Date

import FFI.Date (epoch, getTime) as Date

import FFI.Math as Math

import Data.IPv4 (IPv4(..))
import Data.TCP.Flag (Flag(..))

import Data.Flow as Flow 

range :: Int -> Int -> Effect Int
range min max = do
  w <- Math.random
  x <- pure $ Int.toNumber min
  y <- pure $ Int.toNumber max
  z <- pure $ w * (y - x) + x 
  pure $ Math.floor z

octet :: Effect Int
octet = range 0 255

port :: Effect Int
port = range 0 65535

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
  sPort     <- port
  dIP       <- ipv4
  dPort     <- port
  packets   <- range 0 999999999
  bytes     <- range 0 999999999
  protocol  <- octet
  flags     <- pure $ [U true,R true ,F true,S true,P true,A true]
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
