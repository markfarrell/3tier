module Data.Quote
  ( Quote(..)
  , negate
  ) where

data Quote = Single | Double

negate :: Quote -> Quote
negate Single = Double
negate Double = Single
