module Audit
  ( EventType(..)
  , EventID(..)
  , Entry(..)
  , insert
  , application
  ) where

import Prelude

import Control.Monad.Trans.Class (lift)

import Data.Tuple (Tuple(..))

import Effect.Aff (Aff)
import Effect.Class (liftEffect)

import DB as DB

import Date as Date
import HTTP as HTTP
import Socket as Socket

import UUIDv1 as UUIDv1
import UUIDv5 as UUIDv5

data EventType = Success | Failure

data EventID = DatabaseRequest | ResourceRequest | ResourceResponse | RoutingRequest

data Entry = Entry EventType EventID String

instance showEventType :: Show EventType where
  show Success = "SUCCESS"
  show Failure = "FAILURE"

instance showEventID :: Show EventID where
  show DatabaseRequest  = "DATABASE-REQUEST"
  show ResourceRequest  = "RESOURCE-REQUEST"
  show ResourceResponse = "RESOURCE-RESPONSE"
  show RoutingRequest   = "ROUTING-REQUEST"

instance showEntry :: Show Entry where
  show (Entry eventType eventID msg) = "(Entry " <> show eventType <> " " <> show eventID <> " " <> show msg <> ")"

table :: DB.Table
table = "Audit"

insert :: DB.Database -> Number -> Entry -> HTTP.IncomingMessage -> DB.Request Unit
insert filename duration (Entry eventType eventID msg) req = do
  timestamp <- lift $ liftEffect $ (Date.toISOString <$> Date.current)
  DB.insert filename table $ params timestamp
  where 
    params timestamp =
      [ Tuple "LogID" logID
      , Tuple "SourceID" sourceID
      , Tuple "EntryID" entryID
      , Tuple "Timestamp" timestamp 
      , Tuple "SourceAddress" remoteAddress
      , Tuple "SourcePort" remotePort'
      , Tuple "Duration" duration'
      , Tuple "EventType" eventType'
      , Tuple "EventID" eventID'
      , Tuple "Event" msg
      ]
    remoteAddress = Socket.remoteAddress $ HTTP.socket req
    remotePort = Socket.remotePort $ HTTP.socket req
    remotePort' = show remotePort
    sourceID = UUIDv5.namespaceUUID logID $ remoteAddress
    entryID = UUIDv5.namespaceUUID sourceID $ HTTP.messageURL req
    logID = UUIDv1.defaultUUID
    eventType' = show eventType
    eventID' = show eventID
    duration' = show duration

{-- Audit an application-layer event associated with an incoming HTTP request. --}
application :: DB.Database -> Number -> Entry -> HTTP.IncomingMessage -> Aff Unit
application filename duration entry req = do
  _      <- DB.runRequest (insert filename duration entry $ req)
  pure unit
