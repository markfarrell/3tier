module Audit
  ( EventType(..)
  , EventCategory(..)
  , EventID(..)
  , Event(..)
  , ReportType(..)
  , uri
  ) where

import Prelude

import Data.Foldable (intercalate)

import FFI.Date (Date)

data EventType = Success | Failure

data EventID = Forward | Report | Reject 

data EventCategory = Tier1 | Tier2 | Tier3

data ReportType = Source | Duration

data Event = Event 
  { sourceAddress :: String
  , sourcePort    :: Int 
  , eventType     :: EventType
  , eventCategory :: EventCategory
  , eventID       :: EventID
  , startTime     :: Date
  , duration      :: Number
  , endTime       :: Date
  }

instance showEventTypeAudit :: Show EventType where
  show Success = "SUCCESS"
  show Failure = "FAILURE"

instance showEventIDAudit :: Show EventID where
  show Forward = "FORWARD"
  show Report  = "REPORT"
  show Reject  = "REJECT"

instance showEventCategory :: Show EventCategory where
  show Tier1 = "TIER-1"
  show Tier2 = "TIER-2"
  show Tier3 = "TIER-3"

instance eqEventTypeAudit :: Eq EventType where
  eq Success Success = true
  eq Failure Failure = true
  eq _       _       = false

uri :: Event -> String
uri (Event event') = intercalate separator $
  [ event'.sourceAddress
  , show event'.sourcePort
  , show event'.eventType
  , show event'.eventCategory
  , show event'.eventID
  , show event'.duration
  ] 
  where separator = ","
