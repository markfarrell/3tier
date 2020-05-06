module Data.Schema
  ( Schema(..)
  ) where

import Prelude

data Schema = Audit | Alert | Flow | Statistics | Linux | Windows

instance eqSchemaData :: Eq Schema where
  eq Audit      Audit      = true
  eq Alert      Alert      = true
  eq Flow       Flow       = true
  eq Statistics Statistics = true
  eq Linux      Linux      = true
  eq Windows    Windows    = true
  eq _          _          = false
