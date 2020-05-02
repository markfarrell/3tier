module Data.Risk
  ( Risk(..)
  ) where

import Prelude

data Risk = Injection 

instance showRiskData :: Show Risk where
  show Injection = "???"

instance eqRiskData :: Eq Risk where
  eq Injection Injection = true
