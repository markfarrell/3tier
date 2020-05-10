module Data.Statistics
  ( Event(..)
  , minimum
  , maximum
  , sum
  , total
  , average
  , variance
  ) where

import Prelude

import Foreign (Foreign)

import FFI.JSON as JSON

import Unsafe.Coerce (unsafeCoerce)

data Event = Event
  { min           :: Int
  , max           :: Int
  , sum           :: Int
  , total         :: Int
  , average       :: Int
  , variance      :: Int 
  }

instance showEventStatistics :: Show Event where
  show = JSON.stringify <<< foreignEvent

derive instance eqEventStatistics :: Eq Event 

foreignEvent :: Event -> Foreign
foreignEvent (Event x) = unsafeCoerce $
  { min      : show x.min
  , max      : show x.max
  , sum      : show x.sum
  , total    : show x.total
  , average  : show x.average
  , variance : show x.variance
  }

minimum :: Event -> Int
minimum (Event x) = x.min

maximum :: Event -> Int
maximum (Event x) =  x.max

sum :: Event -> Int
sum (Event x) = x.sum

total :: Event -> Int
total (Event x) = x.total

average :: Event -> Int
average (Event x) = x.average

variance :: Event -> Int
variance (Event x) = x.variance
