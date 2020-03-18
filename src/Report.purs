module Report
  ( Entry(..)
  , ReportType(..)
  , Report(..)
  ) where

import Prelude

import Audit as Audit

data ReportType = Events | Durations

data Report = Audit Audit.EventID Audit.EventType ReportType

data Entry = Entry
  { min                 :: Number
  , max                 :: Number
  , sum                 :: Number
  , total               :: Number
  , average             :: Number
  , variance            :: Number 
  }

instance showReportType :: Show ReportType where
  show Events    = "Events"
  show Durations = "Durations"

instance showReport :: Show Report where
  show (Audit eventID eventType reportType) = "(Audit " <> show eventID <> " " <> show eventType <> " " <> show reportType <> ")" 

instance showEntry :: Show Entry where
  show (Entry entry) = "(Entry " <> show entry <> ")"

instance eqEntry :: Eq Entry where
  eq (Entry x) (Entry y) = x == y
