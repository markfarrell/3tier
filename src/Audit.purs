module Audit
  ( EventType(..)
  , EventID(..)
  , Entry(..)
  , insert
  , application
  , debug
  ) where

import Prelude

import Control.Monad.Error.Class (try)
import Control.Monad.Trans.Class (lift)

import Data.Either(Either(..))
import Data.Tuple(Tuple(..))

import Effect.Aff (Aff)
import Effect.Console (error) as Console
import Effect.Class (liftEffect)

import DB as DB
import Date as Date
import HTTP as HTTP
import Socket as Socket
import Strings as Strings

import UUIDv1 as UUIDv1
import UUIDv3 as UUIDv3

data EventType = Success | Failure

data EventID = DatabaseRequest | ResourceRequest | ResourceResponse | RoutingRequest | AuthorizationRequest | ClientRequest | DeserializationRequest | SerializationRequest | AuditRequest | AssertRequest

data Entry = Entry EventType EventID String

instance showEventType :: Show EventType where
  show Success = "SUCCESS"
  show Failure = "FAILURE"

instance showEventID :: Show EventID where
  show DatabaseRequest = "DATABASE-REQUEST"
  show ResourceRequest = "RESOURCE-REQUEST"
  show ResourceResponse = "RESOURCE-RESPONSE"
  show RoutingRequest = "ROUTING-REQUEST"
  show AuthorizationRequest = "AUTHORIZATION-REQUEST"
  show ClientRequest = "CLIENT-REQUEST"
  show DeserializationRequest = "DESERIALIZATION-REQUEST"
  show SerializationRequest = "SERIALIZATION-REQUEST"
  show AuditRequest = "AUDIT-REQUEST"
  show AssertRequest = "ASSERT-REQUEST"

instance showEntry :: Show Entry where
  show (Entry eventType eventID msg) = "(Entry " <> show eventType <> " " <> show eventID <> " " <> show msg <> ")"

insert :: String -> Entry -> HTTP.IncomingMessage -> DB.Request Unit
insert filename (Entry eventType eventID msg) req = do
  timestamp <- lift $ liftEffect $ (Date.toISOString <$> Date.current)
  DB.insert filename table $ params timestamp
  where 
    params timestamp =
      [ Tuple "Timestamp" timestamp 
      , Tuple "RemoteAddress" remoteAddress
      , Tuple "RemotePort" remotePort'
      , Tuple "LogID" logID
      , Tuple "EntryID" entryID
      , Tuple "EventType" eventType'
      , Tuple "EventID" eventID'
      , Tuple "Message" message
      ]
    remoteAddress = Socket.remoteAddress $ HTTP.socket req
    remotePort = Socket.remotePort $ HTTP.socket req
    remotePort' = show remotePort
    logID = UUIDv1.defaultUUID
    entryID = UUIDv3.namespaceUUID logID $ HTTP.messageURL req
    eventType' = show eventType
    eventID' = show eventID
    message = Strings.encodeBase64 msg
    table = "Audit"

debugFailure :: Entry -> Aff Unit
debugFailure (Entry ty id msg) = do
 timestamp <- liftEffect $ Date.toISOString <$> Date.current
 error' $ show [timestamp, show ty, show id, msg]
 where error' = liftEffect <<< Console.error

debug :: Entry -> Aff Unit
debug = \entry -> do
  case entry of
    (Entry Failure _ _) -> debugFailure entry
    (Entry Success _ _) -> pure unit

{-- Audit an application-layer event associated with an incoming HTTP request. --}
application :: String -> Entry -> HTTP.IncomingMessage -> Aff Unit
application filename entry req = do
  _      <- debug entry
  result <- try $ DB.runRequest (insert filename entry $ req)
  msg    <- pure (show { entry : entry, result : result })
  case result of 
    (Left  _)  -> debug $ Entry Failure AuditRequest msg
    (Right _)  -> debug $ Entry Success AuditRequest msg
