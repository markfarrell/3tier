module Audit
  ( EventType(..)
  , EventCategory(..)
  , EventClass(..)
  , EventInstance(..)
  , EventID(..)
  , EventSource(..)
  , Entry(..)
  , ReportType(..)
  , entry
  , uri
  ) where

import Prelude

import HTTP as HTTP
import Socket as Socket

import Arrays as Arrays

data EventType = Success | Failure

data EventCategory = DatabaseRequest | ResourceRequest | RoutingRequest

data EventClass = Audit | Flow

data EventInstance = Forward EventClass | Report EventClass

type EventID = Array EventInstance

data EventSource = Tier1 | Tier2 | Tier3

data ReportType = Sources | Durations

data Entry = Entry 
  { sourceAddress :: String
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

instance showEventInstance :: Show EventInstance where
  show (Forward Flow)  = "FORWARD-FLOW"
  show (Forward Audit) = "FORWARD-AUDIT"
  show (Report  Flow)  = "REPORT-FLOW"
  show (Report Audit)  = "REPORT-AUDIT"

instance showEventSource :: Show EventSource where
  show Tier1 = "TIER-1"
  show Tier2 = "TIER-2"
  show Tier3 = "TIER-3"

entry :: EventSource -> EventType -> EventCategory -> Number -> EventID -> HTTP.IncomingMessage -> Entry
entry eventSource eventType eventCategory duration eventID req = Entry $
  { sourceAddress : sourceAddress
  , sourcePort    : sourcePort
  , eventType     : eventType
  , eventCategory : eventCategory 
  , duration      : duration
  , eventID       : eventID
  , eventSource   : eventSource
  }
  where
    sourceAddress = Socket.remoteAddress $ HTTP.socket req
    sourcePort = Socket.remotePort $ HTTP.socket req

uri :: Entry -> String
uri (Entry entry') = Arrays.join "," $
  [ entry'.sourceAddress
  , show entry'.sourcePort
  , show entry'.eventType
  , show entry'.eventCategory
  , show entry'.eventID
  , show entry'.eventSource
  , show entry'.duration
  ] 
