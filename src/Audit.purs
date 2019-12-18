module Audit
  ( EventType(..)
  , EventID(..)
  , Entry(..)
  , entryQuery
  ) where

import Prelude

data EventType = Success | Failure

data EventID = DatabaseRequest | ResourceRequest | ResourceResponse | RoutingRequest

data Entry = Entry EventType EventID String

instance showEventType :: Show EventType where
  show Success = "SUCCESS"
  show Failure = "FAILURE"

instance showEventID :: Show EventID where
  show DatabaseRequest = "DATABASE-REQUEST"
  show ResourceRequest    = "RESOURCE-REQUEST"
  show ResourceResponse   = "RESOURCE-RESPONSE"
  show RoutingRequest      = "ROUTING-REQUEST"

instance showEntry :: Show Entry where
  show (Entry eventType eventID msg) = "Entry " <> show eventType <> " " <> show eventID <> " " <> show msg

entryQuery :: String -> String -> Entry -> String
entryQuery uuid sourceHost (Entry eventType eventID msg) = "INSERT INTO Audit (UUID, SourceHost, EventType, EventID, Message) VALUES (" <> values <> ")"
  where values   = "'" <> uuid <> "','" <> sourceHost <> "','" <> show eventType <> "','" <> show eventID  <> "','" <> msg <> "'"
