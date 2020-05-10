module Data.Schema
  ( Schema(..)
  ) where

import Prelude

data Schema = Audit

derive instance eqSchema :: Eq Schema
