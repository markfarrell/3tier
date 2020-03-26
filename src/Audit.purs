module Audit
  ( EventType(..)
  , EventCategory(..)
  , EventID
  , EventSource(..)
  , Entry(..)
  , entry
  ) where

import Prelude

import HTTP as HTTP
import Socket as Socket

data EventType = Success | Failure

data EventCategory = DatabaseRequest | ResourceRequest | RoutingRequest

type EventID = String

data EventSource = Tier1 | Tier2 | Tier3

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
