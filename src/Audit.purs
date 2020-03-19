module Audit
  ( EventType(..)
  , EventID(..)
  , Entry(..)
  ) where

import Prelude

data EventType = Success | Failure

data EventID = DatabaseRequest | ResourceRequest | RoutingRequest

data Entry = Entry EventType EventID Number String

instance showEventType :: Show EventType where
  show Success = "SUCCESS"
  show Failure = "FAILURE"

instance showEventID :: Show EventID where
  show DatabaseRequest  = "DATABASE-REQUEST"
  show ResourceRequest  = "RESOURCE-REQUEST"
  show RoutingRequest   = "ROUTING-REQUEST"

instance showEntry :: Show Entry where
  show (Entry eventType eventID duration msg) = "(Entry " <> show eventType <> " " <> show eventID <> " " <> show duration <> " " <> show msg <> ")"
