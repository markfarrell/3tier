module Audit
  ( EventType(..)
  , EventID(..)
  , Entry(..)
  , insert
  ) where

import Prelude

import Control.Monad.Trans.Class (lift)

import Effect (Effect)
import Effect.Class (liftEffect)

import DB as DB
import Date as Date
import HTTP as HTTP
import Socket as Socket
import UUIDv3 as UUIDv3

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

entryQuery :: Entry -> HTTP.IncomingMessage -> Effect String
entryQuery (Entry eventType eventID msg) req = do
  timestamp <- Date.now
  pure $ query timestamp 
  where
    query timestamp  = "INSERT INTO Audit (UUID, Timestamp, RemoteAddress, RemotePort, EventType, EventID, Message) VALUES (" <> values timestamp <> ")"
    values timestamp = "'" <> uuid <> "','" <> show timestamp <> "','" <> remoteAddress <> "','" <> show remotePort <> "','" <> show eventType <> "','" <> show eventID  <> "','" <> msg <> "'"
    uuid = UUIDv3.url $ HTTP.messageURL req
    remoteAddress = Socket.remoteAddress $ HTTP.socket req
    remotePort = Socket.remotePort $ HTTP.socket req

insert :: Entry -> HTTP.IncomingMessage -> DB.Request Unit
insert entry = \req -> do
  query <- lift $ liftEffect $ entryQuery entry req
  DB.insert filename query
  where filename = "audit.db"
