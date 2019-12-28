module Audit
  ( EventType(..)
  , EventID(..)
  , Entry(..)
  , insert
  , audit
  , log
  ) where

import Prelude

import Control.Monad.Error.Class (try)
import Control.Monad.Trans.Class (lift)

import Data.Either(Either(..))
import Data.Foldable (foldl)

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console (log) as Console
import Effect.Class (liftEffect)

import DB as DB
import Date as Date
import HTTP as HTTP
import Socket as Socket
import Strings as Strings

data EventType = Success | Failure

data EventID = DatabaseRequest | ResourceRequest | ResourceResponse | RoutingRequest | ClientRequest | AuditRequest

data Entry = Entry EventType EventID String

instance showEventType :: Show EventType where
  show Success = "SUCCESS"
  show Failure = "FAILURE"

instance showEventID :: Show EventID where
  show DatabaseRequest = "DATABASE-REQUEST"
  show ResourceRequest = "RESOURCE-REQUEST"
  show ResourceResponse = "RESOURCE-RESPONSE"
  show RoutingRequest = "ROUTING-REQUEST"
  show ClientRequest = "CLIENT-REQUEST"
  show AuditRequest = "AUDIT-REQUEST"

instance showEntry :: Show Entry where
  show (Entry eventType eventID msg) = "(Entry " <> show eventType <> " " <> show eventID <> " " <> show msg <> ")"

entryQuery :: Entry -> HTTP.IncomingMessage -> Effect String
entryQuery (Entry eventType eventID msg) req = do
  timestamp <- Date.toISOString <$> Date.current
  pure $ query timestamp 
  where
    query timestamp  = "INSERT INTO Audit (Timestamp, RemoteAddress, RemotePort, URL, EventType, EventID, Message) VALUES ('" <> values timestamp <> "')"
    values timestamp = foldl (\x y -> x <> "','" <> y) timestamp $ [remoteAddress, remotePort', url', eventType', eventID', message] 
    remoteAddress = Socket.remoteAddress $ HTTP.socket req
    remotePort = Socket.remotePort $ HTTP.socket req
    remotePort' = show remotePort
    url' = Strings.encodeBase64 $ HTTP.messageURL req
    eventType' = show eventType
    eventID' = show eventID
    message = Strings.encodeBase64 msg

insert :: Entry -> HTTP.IncomingMessage -> DB.Request Unit
insert entry = \req -> do
  query <- lift $ liftEffect $ entryQuery entry req
  DB.insert filename query
  where filename = "audit.db"

log' :: String -> Aff Unit
log' = liftEffect <<< Console.log

log :: Entry -> Aff Unit
log (Entry ty id msg) = do
 timestamp <- liftEffect $ Date.toISOString <$> Date.current
 log' $ show [timestamp, show ty, show id, msg]

audit :: Entry -> HTTP.IncomingMessage -> Aff Unit
audit entry req = do
  result <- try $ DB.runRequest (insert entry $ req)
  msg    <- pure (show { entry : entry, result : result })
  case result of 
    (Left  _)  -> log  $ Entry Failure AuditRequest msg
    (Right _)  -> log  $ Entry Success AuditRequest msg
