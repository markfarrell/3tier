module Audit
  ( EventType(..)
  , EventID(..)
  , Entry(..)
  , entry
  ) where

import Prelude

import HTTP as HTTP
import Socket as Socket

data EventType = Success | Failure

data EventID = DatabaseRequest | ResourceRequest | RoutingRequest

data Entry = Entry 
  { sourceIP   :: String
  , sourcePort :: Int 
  , eventType  :: EventType
  , eventID    :: EventID
  , duration   :: Number
  , event      :: String
  }

instance showEventType :: Show EventType where
  show Success = "SUCCESS"
  show Failure = "FAILURE"

instance showEventID :: Show EventID where
  show DatabaseRequest  = "DATABASE-REQUEST"
  show ResourceRequest  = "RESOURCE-REQUEST"
  show RoutingRequest   = "ROUTING-REQUEST"

entry :: EventType -> EventID -> Number -> String -> HTTP.IncomingMessage -> Entry
entry eventType eventID duration event req = Entry $
  { sourceIP : sourceIP
  , sourcePort : sourcePort
  , eventType : eventType
  , eventID : eventID 
  , duration : duration
  , event : event
  }
  where
    sourceIP = Socket.remoteAddress $ HTTP.socket req
    sourcePort = Socket.remotePort $ HTTP.socket req
