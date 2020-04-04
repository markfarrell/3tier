module Audit
  ( EventType(..)
  , EventCategory(..)
  , EventClass(..)
  , EventInstance(..)
  , EventID(..)
  , EventSource(..)
  , Event(..)
  , ReportType(..)
  , uri
  ) where

import Prelude

import Data.Foldable (intercalate)

import FFI.Date (Date)
import FFI.HTTP as HTTP
import FFI.Socket as Socket

data EventType = Success | Failure

data EventCategory = DatabaseRequest | ResourceRequest | RoutingRequest

data EventClass = Audit | Flow

data EventInstance = Forward EventClass | Report EventClass

type EventID = Array EventInstance

data EventSource = Tier1 | Tier2 | Tier3

data ReportType = Sources | Durations

data Event = Event 
  { sourceAddress :: String
  , sourcePort    :: Int 
  , eventType     :: EventType
  , eventCategory :: EventCategory
  , eventID       :: EventID
  , eventSource   :: EventSource
  , startTime     :: Date
  , duration      :: Number
  , endTime       :: Date
  }

instance showEventTypeAudit :: Show EventType where
  show Success = "SUCCESS"
  show Failure = "FAILURE"

instance showEventCategoryAudit :: Show EventCategory where
  show DatabaseRequest  = "DATABASE-REQUEST"
  show ResourceRequest  = "RESOURCE-REQUEST"
  show RoutingRequest   = "ROUTING-REQUEST"

instance showEventInstanceAudit :: Show EventInstance where
  show (Forward Flow)  = "FORWARD-FLOW"
  show (Forward Audit) = "FORWARD-AUDIT"
  show (Report  Flow)  = "REPORT-FLOW"
  show (Report Audit)  = "REPORT-AUDIT"

instance showEventSourceAudit :: Show EventSource where
  show Tier1 = "TIER-1"
  show Tier2 = "TIER-2"
  show Tier3 = "TIER-3"

uri :: Event -> String
uri (Event event') = intercalate separator $
  [ event'.sourceAddress
  , show event'.sourcePort
  , show event'.eventType
  , show event'.eventCategory
  , show event'.eventID
  , show event'.eventSource
  , show event'.duration
  ] 
  where separator = ","
