module Audit
  ( EventType(..)
  , EventID(..)
  , Entry(..)
  , insert
  , schema
  , application
  ) where

import Prelude

import Control.Monad.Error.Class (try)
import Control.Monad.Trans.Class (lift)

import Data.Tuple(Tuple(..))

import Effect.Aff (Aff)
import Effect.Class (liftEffect)

import DB as DB
import Date as Date
import HTTP as HTTP
import Socket as Socket
import Strings as Strings

import UUIDv1 as UUIDv1
import UUIDv3 as UUIDv3

data EventType = Success | Failure

data EventID = DatabaseRequest | ResourceRequest | ResourceResponse | RoutingRequest

data Entry = Entry EventType EventID String

instance showEventType :: Show EventType where
  show Success = "SUCCESS"
  show Failure = "FAILURE"

instance showEventID :: Show EventID where
  show DatabaseRequest = "DATABASE-REQUEST"
  show ResourceRequest = "RESOURCE-REQUEST"
  show ResourceResponse = "RESOURCE-RESPONSE"
  show RoutingRequest = "ROUTING-REQUEST"

instance showEntry :: Show Entry where
  show (Entry eventType eventID msg) = "(Entry " <> show eventType <> " " <> show eventID <> " " <> show msg <> ")"

table :: String
table = "Audit"

insert :: String -> Entry -> HTTP.IncomingMessage -> DB.Request Unit
insert filename (Entry eventType eventID msg) req = do
  timestamp <- lift $ liftEffect $ (Date.toISOString <$> Date.current)
  DB.insert filename table $ params timestamp
  where 
    params timestamp =
      [ Tuple "Timestamp" timestamp 
      , Tuple "SourceAddress" remoteAddress
      , Tuple "SourcePort" remotePort'
      , Tuple "SourceID" sourceID
      , Tuple "EntryID" entryID
      , Tuple "EventType" eventType'
      , Tuple "EventID" eventID'
      , Tuple "Message" message
      ]
    remoteAddress = Socket.remoteAddress $ HTTP.socket req
    remotePort = Socket.remotePort $ HTTP.socket req
    remotePort' = show remotePort
    sourceID = UUIDv3.namespaceUUID UUIDv1.defaultUUID $ remoteAddress
    entryID = UUIDv3.namespaceUUID sourceID $ HTTP.messageURL req
    eventType' = show eventType
    eventID' = show eventID
    message = Strings.encodeBase64 msg

schema :: String -> DB.Request Unit
schema filename = DB.schema filename table $
  [ Tuple "Timestamp" DB.TextNotNull
  , Tuple "SourceAddress" DB.TextNotNull
  , Tuple "SourcePort" DB.TextNotNull
  , Tuple "SourceID" DB.TextNotNull
  , Tuple "EntryID" DB.TextNotNull
  , Tuple "EventType" DB.TextNotNull
  , Tuple "EventID" DB.TextNotNull
  , Tuple "Message" DB.TextNotNull
  ]

{-- Audit an application-layer event associated with an incoming HTTP request. --}
application :: String -> Entry -> HTTP.IncomingMessage -> Aff Unit
application filename entry req = do
  _      <- try $ DB.runRequest (insert filename entry $ req)
  pure unit
