module Effect.Statistics
  ( random
  ) where

import Prelude

import Effect (Effect)

import FFI.Number as Number

import Data.Statistics as Statistics 

import Effect.Range (random) as Range

random :: Effect Statistics.Event
random = do
  min      <- Range.random 0 Number.maxSafeInteger
  max      <- Range.random 0 Number.maxSafeInteger
  sum      <- Range.random 0 Number.maxSafeInteger
  total    <- Range.random 0 Number.maxSafeInteger
  average  <- Range.random 0 Number.maxSafeInteger
  variance <- Range.random 0 Number.maxSafeInteger
  pure $ Statistics.Event $
    { min           : min
    , max           : max
    , sum           : sum
    , total         : total
    , average       : average
    , variance      : variance
    }
