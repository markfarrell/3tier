module Tier2 
  ( Settings(..)
  , execute
  , process
  ) where 
  
import Prelude

import Control.Coroutine (Producer, Consumer, Process, pullFrom, await)
import Control.Coroutine.Aff (produce, emit)
import Control.Monad.Error.Class (try)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)

import Data.Either (Either(..))

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)

import Data.Tuple (Tuple(..))

import Unsafe.Coerce (unsafeCoerce)

import FFI.Date as Date
import FFI.HTTP as HTTP
import FFI.Socket as Socket
import FFI.JSON as JSON

import Tier3 as Tier3

import Audit as Audit

import Forward (Forward)
import Forward as Forward

import Report (Report)
import Report as Report

import Route (Route)
import Route as Route

data Settings = Settings { host :: String, port :: Int }

data ContentType a = TextJSON a

data AuthenticationType = Bearer

data Resource a = Ok (ContentType a) | InternalServerError String | BadRequest String | Forbidden AuthenticationType String

audit :: Tier3.Settings -> Audit.Event -> Tier3.Request Unit
audit settings event = do
  _ <- Tier3.request settings $ Route.Forward (Forward.Audit event)
  pure unit

databaseRequest :: Tier3.Settings -> Route -> HTTP.IncomingMessage -> Aff (Resource Tier3.ResultSet) 
databaseRequest settings route req = do
  startTime <- liftEffect $ Date.current
  result    <- Tier3.execute $ Tier3.request settings route
  endTime   <- liftEffect $ Date.current
  duration  <- pure $ (Date.getTime endTime) - (Date.getTime startTime)
  eventType <- pure $ case result of
                 (Left _)  -> Audit.Failure
                 (Right _) -> Audit.Success
  eventID <- pure $ case result of
                 (Left _)                  -> []
                 (Right (Tuple _ eventID)) -> eventID
  event     <- pure $ Audit.Event $
                 { eventSource   : Audit.Tier2
                 , eventType     : eventType
                 , eventCategory : Audit.DatabaseRequest
                 , eventID       : eventID
                 , startTime     : startTime
                 , duration      : duration
                 , endTime       : endTime
                 , sourceAddress : Socket.remoteAddress $ HTTP.socket req
                 , sourcePort    : Socket.remotePort    $ HTTP.socket req
                 }
  _         <- Tier3.execute $ audit settings event
  case result of 
    (Left _)                    -> pure  $ InternalServerError ""
    (Right (Tuple resultSet _)) -> pure  $ Ok (TextJSON resultSet)

routingRequest :: Tier3.Settings -> HTTP.IncomingMessage -> Aff (Either Error Route)
routingRequest settings req = do 
  startTime <- liftEffect $ Date.current
  result    <- Route.execute req
  endTime   <- liftEffect $ Date.current
  duration  <- pure $ (Date.getTime endTime) - (Date.getTime startTime)
  eventType <- pure $ case result of
                 (Left _)  -> Audit.Failure
                 (Right _) -> Audit.Success
  eventID <- pure $ case result of
                 (Left _)      -> []
                 (Right route) -> Route.eventID route
  event     <- pure $ Audit.Event $
                 { eventSource   : Audit.Tier2
                 , eventType     : eventType
                 , eventCategory : Audit.RoutingRequest
                 , eventID       : eventID
                 , startTime     : startTime
                 , duration      : duration
                 , endTime       : endTime
                 , sourceAddress : Socket.remoteAddress $ HTTP.socket req
                 , sourcePort    : Socket.remotePort    $ HTTP.socket req
                 }
  _         <- Tier3.execute $ audit settings event
  pure result


textJSON :: Tier3.ResultSet -> String
textJSON (Tier3.Forward _) = ""
textJSON (Tier3.Report x)  = JSON.stringify $ unsafeCoerce x

sendResource :: Resource Tier3.ResultSet -> HTTP.ServerResponse -> Aff Unit
sendResource (Ok (TextJSON body)) = \res -> liftEffect $ do
  _ <- HTTP.setHeader "Content-Type" "text/json" $ res
  _ <- HTTP.writeHead 200 $ res
  _ <- HTTP.write (textJSON body) $ res
  _ <- HTTP.end $ res
  pure unit
sendResource (BadRequest _) = \res -> liftEffect $ do
  _ <- HTTP.writeHead 400 $ res
  _ <- HTTP.end $ res
  pure unit  
sendResource (Forbidden Bearer realm) = \res -> liftEffect $ do
  _ <- HTTP.setHeader "WWW-Authenticate" ("Bearer realm=" <> realm) $ res
  _ <- HTTP.writeHead 401 $ res
  _ <- HTTP.end $ res
  pure unit  
sendResource (InternalServerError _) = \res -> liftEffect $ do
  _ <- HTTP.writeHead 500 $ res
  _ <- HTTP.end $ res
  pure unit

resourceRequest :: Tier3.Settings -> HTTP.IncomingRequest -> Aff Unit
resourceRequest settings (HTTP.IncomingRequest req res) = do
  startTime     <- liftEffect $ Date.current
  routingResult <- routingRequest settings req
  resource      <- case routingResult of
                     (Left _)      -> pure $ BadRequest (HTTP.messageURL req)
                     (Right route) -> databaseRequest settings route req 
  response      <- try $ sendResource resource res 
  endTime       <- liftEffect $ Date.current
  duration      <- pure $ (Date.getTime endTime) - (Date.getTime startTime)
  eventID       <- pure $ case routingResult of
                     (Left _)      -> []
                     (Right route) -> Route.eventID route
  eventType     <- pure $ case routingResult of
                     (Left  _) -> Audit.Failure
                     (Right _) -> case resource of
                       (Ok _) -> Audit.Success
                       _      -> case response of
                         (Left _)  -> Audit.Failure
                         (Right _) -> Audit.Success
  event     <- pure $ Audit.Event $
                 { eventSource   : Audit.Tier2
                 , eventType     : eventType
                 , eventCategory : Audit.ResourceRequest
                 , eventID       : eventID
                 , startTime     : startTime
                 , duration      : duration
                 , endTime       : endTime
                 , sourceAddress : Socket.remoteAddress $ HTTP.socket req
                 , sourcePort    : Socket.remotePort    $ HTTP.socket req
                 }
  _     <- Tier3.execute $ audit settings event
  pure unit

producer :: HTTP.Server -> Producer HTTP.IncomingRequest Aff Unit
producer server = produce \emitter -> do
  HTTP.onRequest (\req res -> emit emitter $ HTTP.IncomingRequest req res) $ server

consumer :: Tier3.Settings -> Consumer HTTP.IncomingRequest Aff Unit
consumer settings = forever $ do
  request <- await
  _       <- lift $ resourceRequest settings request
  pure unit

process :: Tier3.Settings -> HTTP.Server -> Process Aff Unit
process settings server = pullFrom (consumer settings) (producer server)

url :: Settings -> String -> String
url (Settings settings) uri = location <> uri
  where location = "http://" <> settings.host <> ":" <> show settings.port

executeForward :: Settings -> Forward -> Aff (Either Error HTTP.IncomingResponse)
executeForward settings query = do
  req <- HTTP.createRequest HTTP.Post $ url settings (Forward.uri query)
  res <- try $ HTTP.endRequest req
  pure res

executeReport :: Settings -> Report -> Aff (Either Error HTTP.IncomingResponse)
executeReport settings query = do
  req <- HTTP.createRequest HTTP.Get $ url settings (Report.uri query)
  res <- try $ HTTP.endRequest req
  pure res

execute :: Settings -> Route -> Aff (Either Error HTTP.IncomingResponse)
execute settings (Route.Forward query) = executeForward settings query
execute settings (Route.Report query)  = executeReport settings query
