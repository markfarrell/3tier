module Text.Parsing.Statistics
  ( event
  ) where

import Prelude

import Text.Parsing.Parser (Parser)

import Data.Statistics as Statistics

import Text.Parsing.Common (json, property, nonnegativeInteger)

event :: Parser String Statistics.Event
event = do
  x        <- json
  min      <- property "min"      x $ nonnegativeInteger
  max      <- property "max"      x $ nonnegativeInteger
  sum      <- property "sum"      x $ nonnegativeInteger
  total    <- property "total"    x $ nonnegativeInteger
  average  <- property "average"  x $ nonnegativeInteger
  variance <- property "variance" x $ nonnegativeInteger
  pure $ Statistics.Event $
    { min      : min
    , max      : max
    , sum      : sum
    , total    : total
    , average  : average
    , variance : variance
    }
