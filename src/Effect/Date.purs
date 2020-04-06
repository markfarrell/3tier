module Effect.Date
  ( random
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Int as Int

import Effect (Effect)

import FFI.Math as Math

import FFI.Date (Date)
import FFI.Date as Date

range :: Int -> Int -> Effect Int
range min max = do
  w <- Math.random
  x <- pure $ Int.toNumber min
  y <- pure $ Int.toNumber max
  z <- pure $ w * (y - x) + x 
  pure $ Math.floor z

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

random :: Effect Date
random = do
  x <- isoString
  y <- pure $ Date.parse x
  case y of
    (Left _)  -> pure Date.epoch
    (Right z) -> pure z

