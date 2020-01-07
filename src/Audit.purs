module Audit
  ( EventType(..)
  , EventID(..)
  , Entry(..)
  , insert
  , audit
  , debug
  ) where

import Prelude

import Control.Monad.Error.Class (try)
import Control.Monad.Trans.Class (lift)

import Data.Either(Either(..))
import Data.Foldable (foldl)

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (error) as Console
import Effect.Class (liftEffect)

import DB as DB
import Date as Date
import HTTP as HTTP
import Socket as Socket
import Strings as Strings

import UUIDv1 as UUIDv1

data EventType = Success | Failure

data EventID = DatabaseRequest | ResourceRequest | ResourceResponse | RoutingRequest | AuthorizationRequest | ClientRequest | DeserializationRequest | SerializationRequest | AuditRequest

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

instance showEntry :: Show Entry where
  show (Entry eventType eventID msg) = "(Entry " <> show eventType <> " " <> show eventID <> " " <> show msg <> ")"

entryQuery :: Entry -> HTTP.IncomingMessage -> Effect String
entryQuery (Entry eventType eventID msg) req = do
  timestamp <- Date.toISOString <$> Date.current
  pure $ query timestamp 
  where
    query timestamp  = "INSERT INTO Audit (Timestamp, RemoteAddress, RemotePort, LogID, EntryID, EventType, EventID, Message) VALUES ('" <> values timestamp <> "')"
    values timestamp = foldl (\x y -> x <> "','" <> y) timestamp $ [remoteAddress, remotePort', logID, entryID, eventType', eventID', message] 
    remoteAddress = Socket.remoteAddress $ HTTP.socket req
    remotePort = Socket.remotePort $ HTTP.socket req
    remotePort' = show remotePort
    logID = UUIDv1.defaultUUID
    entryID = Strings.encodeBase64 $ HTTP.messageURL req
    eventType' = show eventType
    eventID' = show eventID
    message = Strings.encodeBase64 msg

insert :: Entry -> HTTP.IncomingMessage -> DB.Request Unit
insert entry = \req -> do
  query <- lift $ liftEffect $ entryQuery entry req
  DB.insert filename query
  where filename = "audit.db"

debug' :: Entry -> Aff Unit
debug' (Entry ty id msg) = do
 timestamp <- liftEffect $ Date.toISOString <$> Date.current
 error' $ show [timestamp, show ty, show id, msg]
 where error' = liftEffect <<< Console.error

debug :: Entry -> Aff Unit
debug = \entry -> do
  case entry of
    (Entry Failure _ _) -> debug' entry
    (Entry Success _ _) -> pure unit

audit :: Entry -> HTTP.IncomingMessage -> Aff Unit
audit entry req = do
  _      <- debug entry
  result <- try $ DB.runRequest (insert entry $ req)
  msg    <- pure (show { entry : entry, result : result })
  case result of 
    (Left  _)  -> debug $ Entry Failure AuditRequest msg
    (Right _)  -> debug $ Entry Success AuditRequest msg
