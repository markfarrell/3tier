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
import Data.Tuple as Tuple

import Unsafe.Coerce (unsafeCoerce)

import FFI.Date as Date
import FFI.HTTP as HTTP
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

databaseRequest'' :: forall a. Tier3.Result a -> Number -> HTTP.IncomingMessage -> Audit.Event
databaseRequest'' (Left _)                = \duration req -> Audit.event Audit.Tier2 Audit.Failure Audit.DatabaseRequest duration [] $ req
databaseRequest'' (Right (Tuple _ steps)) = \duration req -> Audit.event Audit.Tier2 Audit.Success Audit.DatabaseRequest duration steps $ req

databaseRequest':: Tier3.Settings -> Route -> HTTP.IncomingMessage -> Aff (Resource Tier3.ResultSet) 
databaseRequest' settings route req = do
  startTime <- liftEffect $ Date.currentTime
  result    <- Tier3.execute $ Tier3.request settings route
  endTime   <- liftEffect $ Date.currentTime
  duration  <- pure $ endTime - startTime
  event    <- pure $ databaseRequest'' result duration req
  _         <- Tier3.execute $ audit settings event
  case result of 
    (Left _)                    -> pure  $ InternalServerError ""
    (Right (Tuple resultSet _)) -> pure  $ Ok (TextJSON resultSet)

databaseRequest :: Tier3.Settings -> Route -> HTTP.IncomingMessage -> Aff (Resource Tier3.ResultSet)
databaseRequest settings route req = do
  resource <- databaseRequest' settings route req
  pure resource

routingRequest' :: Either Error Route -> Number -> HTTP.IncomingMessage -> Audit.Event
routingRequest' (Left _)      = \duration req -> Audit.event Audit.Tier2 Audit.Failure Audit.RoutingRequest duration [] $ req 
routingRequest' (Right route) = \duration req -> Audit.event Audit.Tier2 Audit.Success Audit.RoutingRequest duration (Route.eventID route) $ req

routingRequest :: Tier3.Settings -> HTTP.IncomingMessage -> Aff (Either Error Route)
routingRequest settings req = do 
  startTime <- liftEffect $ Date.currentTime
  result    <- Route.execute req
  endTime   <- liftEffect $ Date.currentTime
  duration  <- pure $ endTime - startTime
  event     <- pure $ routingRequest' result duration req
  _         <- Tier3.execute $ audit settings event
  pure result

resourceRequest'' :: forall a b. Either a Route -> Either a (Resource b) -> Number -> HTTP.IncomingMessage -> Audit.Event
resourceRequest'' (Left _) (Left _)                             = \duration req -> Audit.event Audit.Tier2 Audit.Failure Audit.ResourceRequest duration [] $ req
resourceRequest'' (Left _) (Right _)                            = \duration req -> Audit.event Audit.Tier2 Audit.Failure Audit.ResourceRequest duration [] $ req
resourceRequest'' (Right route) (Left _)                        = \duration req -> Audit.event Audit.Tier2 Audit.Failure Audit.ResourceRequest duration (Route.eventID route) $ req
resourceRequest'' (Right route) (Right (Ok _))                  = \duration req -> Audit.event Audit.Tier2 Audit.Success Audit.ResourceRequest duration (Route.eventID route) $ req 
resourceRequest'' (Right route) (Right (InternalServerError _)) = \duration req -> Audit.event Audit.Tier2 Audit.Failure Audit.ResourceRequest duration (Route.eventID route) $ req
resourceRequest'' (Right route) (Right (BadRequest _))          = \duration req -> Audit.event Audit.Tier2 Audit.Failure Audit.ResourceRequest duration (Route.eventID route) $ req
resourceRequest'' (Right route) (Right (Forbidden _ _))         = \duration req -> Audit.event Audit.Tier2 Audit.Failure Audit.ResourceRequest duration (Route.eventID route) $ req

resourceRequest' :: Tier3.Settings -> HTTP.IncomingMessage -> Aff (Tuple (Either Error Route) (Resource Tier3.ResultSet))
resourceRequest' settings req  = do
  result <- routingRequest settings req
  case result of
    (Left  _)      -> do
       resource <- pure $ BadRequest (HTTP.messageURL req)
       pure $ Tuple result resource
    (Right route') -> do
       resource <- databaseRequest settings route' req
       pure $ Tuple result resource

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
  startTime       <- liftEffect $ Date.currentTime
  result          <- resourceRequest' settings req
  result'         <- try $ sendResource (Tuple.snd result) res 
  endTime         <- liftEffect $ Date.currentTime
  duration        <- pure (endTime - startTime)
  case result' of
    (Left error) -> do
      event <- pure $ resourceRequest'' (Tuple.fst result) (Left error) duration req 
      _     <- Tier3.execute $ audit settings event
      pure unit
    (Right _)    -> do
       event <- pure $ resourceRequest'' (Tuple.fst result) (Right (Tuple.snd result)) duration req
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
