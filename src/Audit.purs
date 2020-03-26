module Audit
  ( EventType(..)
  , EventCategory(..)
  , EventID(..)
  , EventSource(..)
  , Entry(..)
  , ReportType(..)
  , entry
  ) where

import Prelude

import HTTP as HTTP
import Socket as Socket

import Arrays as Arrays

data EventType = Success | Failure

data EventCategory = DatabaseRequest | ResourceRequest | RoutingRequest

data EventID = EventID (Array EventID) | ForwardFlow | ReportAudit | Unknown

data EventSource = Tier1 | Tier2 | Tier3

data ReportType = Sources | Durations

data Entry = Entry 
  { sourceIP      :: String
  , sourcePort    :: Int 
  , eventType     :: EventType
  , eventCategory :: EventCategory
  , eventID       :: EventID
  , eventSource   :: EventSource
  , duration      :: Number
  }

instance showEventType :: Show EventType where
  show Success = "SUCCESS"
  show Failure = "FAILURE"

instance showEventCategory :: Show EventCategory where
  show DatabaseRequest  = "DATABASE-REQUEST"
  show ResourceRequest  = "RESOURCE-REQUEST"
  show RoutingRequest   = "ROUTING-REQUEST"

instance showEventID :: Show EventID where
  show ForwardFlow  = "FORWARD-FLOW"
  show ReportAudit  = "REPORT-AUDIT"
  show Unknown      = "???"
  show (EventID x)  = Arrays.join "," (show <$> x)

instance showEventSource :: Show EventSource where
  show Tier1 = "TIER-1"
  show Tier2 = "TIER-2"
  show Tier3 = "TIER-3"

entry :: EventSource -> EventType -> EventCategory -> Number -> EventID -> HTTP.IncomingMessage -> Entry
entry eventSource eventType eventCategory duration eventID req = Entry $
  { sourceIP      : sourceIP
  , sourcePort    : sourcePort
  , eventType     : eventType
  , eventCategory : eventCategory 
  , duration      : duration
  , eventID       : eventID
  , eventSource   : eventSource
  }
  where
    sourceIP = Socket.remoteAddress $ HTTP.socket req
    sourcePort = Socket.remotePort $ HTTP.socket req
