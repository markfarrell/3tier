module Report
  ( Entry(..)
  , Report(..)
  ) where

import Prelude

import Audit as Audit

data Report = Audit Audit.EventCategory Audit.EventType Audit.ReportType

data Entry = Entry
  { min                 :: Number
  , max                 :: Number
  , sum                 :: Number
  , total               :: Number
  , average             :: Number
  , variance            :: Number 
  }

instance eqEntry :: Eq Entry where
  eq (Entry x) (Entry y) = x == y
