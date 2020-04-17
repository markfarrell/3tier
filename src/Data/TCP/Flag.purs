module Data.TCP.Flag
  ( Flag(..)
  ) where

import Prelude

data Flag = U | R | F | S | P | A

instance showFlag :: Show Flag where
  show U = "U"
  show R = "R"
  show F = "F"
  show S = "S"
  show P = "P"
  show A = "A"
