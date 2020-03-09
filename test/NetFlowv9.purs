module Test.NetFlowv9
  ( main 
  ) where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Buffer as Buffer

import NetFlowv9 as NetFlowv9

{-- Source: https://github.com/hroi/netflowv9/blob/master/src/tests.rs --}
example :: Array Int
example =  [0x00, 0x09, 0x00, 0x04, 0x70, 0x59, 0x38, 0x38, 0x57, 0x8b, 0xe0, 0xfb, 0x00, 0x00, 0x03,
  0xb9, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xac, 0x01, 0x01, 0x00, 0x15, 0x00, 0x15,
  0x00, 0x04, 0x00, 0x16, 0x00, 0x04, 0x00, 0x01, 0x00, 0x04, 0x00, 0x02, 0x00, 0x04, 0x00,
  0x0a, 0x00, 0x02, 0x00, 0x0e, 0x00, 0x02, 0x00, 0x08, 0x00, 0x04, 0x00, 0x0c, 0x00, 0x04,
  0x00, 0x04, 0x00, 0x01, 0x00, 0x05, 0x00, 0x01, 0x00, 0x07, 0x00, 0x02, 0x00, 0x0b, 0x00,
  0x02, 0x00, 0x30, 0x00, 0x01, 0x00, 0x33, 0x00, 0x01, 0x00, 0x0f, 0x00, 0x04, 0x00, 0x0d,
  0x00, 0x01, 0x00, 0x09, 0x00, 0x01, 0x00, 0x06, 0x00, 0x01, 0x00, 0x3d, 0x00, 0x01, 0x00,
  0x11, 0x00, 0x02, 0x00, 0x10, 0x00, 0x02, 0x01, 0x00, 0x00, 0x13, 0x00, 0x15, 0x00, 0x04,
  0x00, 0x16, 0x00, 0x04, 0x00, 0x01, 0x00, 0x04, 0x00, 0x02, 0x00, 0x04, 0x00, 0x0a, 0x00,
  0x02, 0x00, 0x0e, 0x00, 0x02, 0x00, 0x08, 0x00, 0x04, 0x00, 0x0c, 0x00, 0x04, 0x00, 0x04,
  0x00, 0x01, 0x00, 0x05, 0x00, 0x01, 0x00, 0x07, 0x00, 0x02, 0x00, 0x0b, 0x00, 0x02, 0x00,
  0x30, 0x00, 0x01, 0x00, 0x33, 0x00, 0x01, 0x00, 0x0f, 0x00, 0x04, 0x00, 0x0d, 0x00, 0x01,
  0x00, 0x09, 0x00, 0x01, 0x00, 0x06, 0x00, 0x01, 0x00, 0x3d, 0x00, 0x01, 0x01, 0x01, 0x00,
  0x64, 0x70, 0x58, 0xd2, 0xb0, 0x70, 0x58, 0xd2, 0xb0, 0x00, 0x00, 0x00, 0x28, 0x00, 0x00,
  0x00, 0x01, 0x00, 0x4c, 0x00, 0x00, 0xc6, 0x14, 0x45, 0x62, 0x53, 0xdd, 0x80, 0x0f, 0x06,
  0x00, 0x79, 0xff, 0x0f, 0xe0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x1e, 0x00, 0x02, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x70, 0x58, 0xe6, 0x70, 0x70, 0x58, 0xe6, 0x70, 0x00, 0x00, 0x00,
  0x2c, 0x00, 0x00, 0x00, 0x01, 0x00, 0x4c, 0x00, 0x00, 0x95, 0x38, 0x03, 0x74, 0x53, 0xdd,
  0x80, 0x0e, 0x06, 0x00, 0x00, 0x50, 0x0f, 0xca, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x1e,
  0x00, 0x12, 0x00, 0x00, 0x00, 0x00, 0x00]

main :: Effect Unit
main = do
  packet   <- Buffer.toIntArray =<< Buffer.from example
  result   <- NetFlowv9.readPacket packet
  _        <- log $ show packet
  _        <- log $ show result
  pure unit
