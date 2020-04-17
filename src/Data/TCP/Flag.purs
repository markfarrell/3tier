module Data.TCP.Flag
  ( Flag(..)
  ) where

import Prelude

data Flag = U Boolean | R Boolean | F Boolean | S Boolean | P Boolean | A Boolean

instance showFlag :: Show Flag where
  show (U true)  = "U"
  show (R true)  = "R"
  show (F true)  = "F"
  show (S true)  = "S"
  show (P true)  = "P"
  show (A true)  = "A"
  show (U false) = ""
  show (R false) = ""
  show (F false) = ""
  show (S false) = ""
  show (P false) = ""
  show (A false) = ""

instance eqFlag :: Eq Flag where
  eq (U x) (U y) = (x == y)
  eq (R x) (R y) = (x == y)
  eq (F x) (F y) = (x == y)
  eq (S x) (S y) = (x == y)
  eq (P x) (P y) = (x == y)
  eq (A x) (A y) = (x == y)
  eq _     _     = false
