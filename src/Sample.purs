module Sample
  ( flow
  ) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe)
import Data.Either (Either(..))

import Effect (Effect)

import FFI.Date (Date)
import FFI.Date as Date

import FFI.Math as Math

import IPv4 (IPv4(..))

import Flow as Flow 

range :: Int -> Int -> Effect Int
range min max = do
  w <- Math.random
  x <- pure $ Int.toNumber min
  y <- pure $ Int.toNumber max
  z <- pure $ w * (y - x) + x 
  pure $ Math.floor z

index :: forall a. Array a -> Effect Int
index x = range 0 (Array.length x) 

array :: forall a. Array a -> Effect (Maybe a)
array x = do
  y <- index x
  pure $ Array.index x y

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

year :: Effect Int
year = do
  w <- Date.current
  x <- pure $ Date.getYear w
  y <- pure $ Math.floor x
  z <- pure $ 1900 + y
  range 1970 z

isoString :: Effect String
isoString = do
  x <- year
  y <- pure $ show x <> "-01-01T00:00:00.000Z"
  pure y 

date :: Effect Date
date = do
  x <- isoString
  y <- pure $ Date.parse x
  case y of
    (Left _)  -> pure Date.epoch
    (Right z) -> pure z

flow :: Effect Flow.Event
flow = do
  sIP       <- ipv4
  sPort     <- port
  dIP       <- ipv4
  dPort     <- port
  packets   <- range 0 999999999
  bytes     <- range 0 999999999
  protocol  <- octet
  flags     <- pure [Flow.U, Flow.R, Flow.F, Flow.S, Flow.P, Flow.A]
  startTime <- pure $ Date.epoch
  endTime   <- date
  duration  <- pure $ (Date.getTime endTime) - (Date.getTime startTime)
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
