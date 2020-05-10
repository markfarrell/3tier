module Effect.Range
  ( random
  ) where

import Prelude

import Data.Int as Int

import Effect (Effect)

import FFI.Math as Math

random :: Int -> Int -> Effect Int
random min max = do
  w <- Math.random
  x <- pure $ Int.toNumber min
  y <- pure $ Int.toNumber max
  z <- pure $ w * (y - x) + x 
  pure $ Math.floor z
