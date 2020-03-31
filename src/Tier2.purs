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
import Effect.Exception as Exception

import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple

import Unsafe.Coerce (unsafeCoerce)

import JSON as JSON
import Strings as Strings

import Tier3 as Tier3

import Date as Date
import HTTP as HTTP

import Audit as Audit
import Flow as Flow

import Forward as Forward

import Report (Report)
import Report as Report

import Route (Route)
import Route as Route

data Settings = Settings { host :: String, port :: Int }

data ContentType a = TextJSON a

data AuthenticationType = Bearer

data Resource a = Ok (ContentType a) | InternalServerError a | BadRequest a | Forbidden AuthenticationType a

instance showContentType :: (Show a) => Show (ContentType a) where
  show (TextJSON x) = "(TextJSON (" <> show x <> "))"

instance showAuthenticationType :: Show AuthenticationType where
  show Bearer = "Bearer"

class ContentJSON a where
  showJSON :: a -> String

instance contentJSONUnit :: ContentJSON Tier3.ResultSet where
  showJSON (Tier3.InsertResult _) = ""
  showJSON (Tier3.SelectResult x) = JSON.stringify $ unsafeCoerce x

audit :: Tier3.Settings -> Audit.Entry -> Tier3.Request Unit
audit settings entry = do
  _ <- Tier3.request settings $ Route.Forward (Forward.Audit entry)
  pure unit

databaseRequest'' :: forall a. Tier3.Result a -> Number -> HTTP.IncomingMessage -> Audit.Entry
databaseRequest'' (Left _)                = \duration req -> Audit.entry Audit.Tier2 Audit.Failure Audit.DatabaseRequest duration [] $ req
databaseRequest'' (Right (Tuple _ steps)) = \duration req -> Audit.entry Audit.Tier2 Audit.Success Audit.DatabaseRequest duration steps $ req

databaseRequest':: Tier3.Settings -> Route -> HTTP.IncomingMessage -> Aff (Resource String) 
databaseRequest' settings route req = do
  startTime <- liftEffect $ Date.currentTime
  result    <- Tier3.execute $ Tier3.request settings route
  endTime   <- liftEffect $ Date.currentTime
  duration  <- pure $ endTime - startTime
  entry     <- pure $ databaseRequest'' result duration req
  _         <- Tier3.execute $ audit settings entry
  case result of 
    (Left _)                    -> pure  $ InternalServerError ""
    (Right (Tuple resultSet _)) -> pure  $ Ok (TextJSON (showJSON resultSet))

databaseRequest :: Tier3.Settings -> Route -> HTTP.IncomingMessage -> Aff (Resource String)
databaseRequest settings route req = do
  resource <- databaseRequest' settings route req
  pure resource

routingRequest' :: Either Error Route -> Number -> HTTP.IncomingMessage -> Audit.Entry
routingRequest' (Left _)      = \duration req -> Audit.entry Audit.Tier2 Audit.Failure Audit.RoutingRequest duration [] $ req 
routingRequest' (Right route) = \duration req -> Audit.entry Audit.Tier2 Audit.Success Audit.RoutingRequest duration (Route.eventID route) $ req

routingRequest :: Tier3.Settings -> HTTP.IncomingMessage -> Aff (Either Error Route)
routingRequest settings req = do 
  startTime <- liftEffect $ Date.currentTime
  result    <- Route.execute req
  endTime   <- liftEffect $ Date.currentTime
  duration  <- pure $ endTime - startTime
  entry     <- pure $ routingRequest' result duration req
  _         <- Tier3.execute $ audit settings entry
  pure result

resourceRequest'' :: forall a b. Either a Route -> Either a (Resource b) -> Number -> HTTP.IncomingMessage -> Audit.Entry
resourceRequest'' (Left _) (Left _)                             = \duration req -> Audit.entry Audit.Tier2 Audit.Failure Audit.ResourceRequest duration [] $ req
resourceRequest'' (Left _) (Right _)                            = \duration req -> Audit.entry Audit.Tier2 Audit.Failure Audit.ResourceRequest duration [] $ req
resourceRequest'' (Right route) (Left _)                        = \duration req -> Audit.entry Audit.Tier2 Audit.Failure Audit.ResourceRequest duration (Route.eventID route) $ req
resourceRequest'' (Right route) (Right (Ok _))                  = \duration req -> Audit.entry Audit.Tier2 Audit.Success Audit.ResourceRequest duration (Route.eventID route) $ req 
resourceRequest'' (Right route) (Right (InternalServerError _)) = \duration req -> Audit.entry Audit.Tier2 Audit.Failure Audit.ResourceRequest duration (Route.eventID route) $ req
resourceRequest'' (Right route) (Right (BadRequest _))          = \duration req -> Audit.entry Audit.Tier2 Audit.Failure Audit.ResourceRequest duration (Route.eventID route) $ req
resourceRequest'' (Right route) (Right (Forbidden _ _))         = \duration req -> Audit.entry Audit.Tier2 Audit.Failure Audit.ResourceRequest duration (Route.eventID route) $ req

resourceRequest' :: Tier3.Settings -> HTTP.IncomingMessage -> Aff (Tuple (Either Error Route) (Resource String))
resourceRequest' settings req  = do
  result <- routingRequest settings req
  case result of
    (Left  _)      -> do
       resource <- pure $ BadRequest (HTTP.messageURL req)
       pure $ Tuple result resource
    (Right route') -> do
       resource <- databaseRequest settings route' req
       pure $ Tuple result resource

sendResource :: Resource String -> HTTP.ServerResponse -> Aff Unit
sendResource (Ok (TextJSON body)) = \res -> liftEffect $ do
  _ <- HTTP.setHeader "Content-Type" "text/json" $ res
  _ <- HTTP.writeHead 200 $ res
  _ <- HTTP.write body $ res
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
      entry <- pure $ resourceRequest'' (Tuple.fst result) (Left error) duration req 
      _     <- Tier3.execute $ audit settings entry
      pure unit
    (Right _)    -> do
       entry <- pure $ resourceRequest'' (Tuple.fst result) (Right (Tuple.snd result)) duration req
       _     <- Tier3.execute $ audit settings entry
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

url' :: Settings -> String -> String
url' (Settings settings) uri = location <> uri
  where location = "http://" <> settings.host <> ":" <> show settings.port

executeForwardFlow :: Settings -> Flow.Entry -> Aff (Either Error HTTP.IncomingResponse)
executeForwardFlow settings entry = do
  result <- pure $ Flow.write entry
  case result of
    (Left error)       -> pure (Left error)
    (Right identifier) -> do
      req <- HTTP.createRequest HTTP.Post $ url' settings ("/forward/flow/" <> Strings.encodeURIComponent identifier)
      res <- try $ HTTP.endRequest req
      pure res

executeReport' :: Settings -> Report -> String
executeReport' settings (Report.Audit Audit.DatabaseRequest Audit.Success Audit.Sources)   = url' settings "/report/audit/database-request/success/sources"  
executeReport' settings (Report.Audit Audit.DatabaseRequest Audit.Failure Audit.Sources)   = url' settings "/report/audit/database-request/success/sources"  
executeReport' settings (Report.Audit Audit.DatabaseRequest Audit.Success Audit.Durations) = url' settings "/report/audit/database-request/success/durations"  
executeReport' settings (Report.Audit Audit.DatabaseRequest Audit.Failure Audit.Durations) = url' settings "/report/audit/database-request/success/durations"  
executeReport' settings (Report.Audit Audit.ResourceRequest Audit.Success Audit.Sources)   = url' settings "/report/audit/resource-request/success/sources"  
executeReport' settings (Report.Audit Audit.ResourceRequest Audit.Failure Audit.Sources)   = url' settings "/report/audit/resource-request/success/sources"  
executeReport' settings (Report.Audit Audit.ResourceRequest Audit.Success Audit.Durations) = url' settings "/report/audit/resource-request/success/durations"  
executeReport' settings (Report.Audit Audit.ResourceRequest Audit.Failure Audit.Durations) = url' settings "/report/audit/resource-request/success/durations"  
executeReport' settings (Report.Audit Audit.RoutingRequest Audit.Success Audit.Sources)    = url' settings "/report/audit/routing-request/success/sources"  
executeReport' settings (Report.Audit Audit.RoutingRequest Audit.Failure Audit.Sources)    = url' settings "/report/audit/routing-request/success/sources"  
executeReport' settings (Report.Audit Audit.RoutingRequest Audit.Success Audit.Durations)  = url' settings "/report/audit/routing-request/success/durations"  
executeReport' settings (Report.Audit Audit.RoutingRequest Audit.Failure Audit.Durations)  = url' settings "/report/audit/routing-request/success/durations"  

executeReport :: Settings -> Report -> Aff (Either Error HTTP.IncomingResponse)
executeReport settings request = do
  req <- HTTP.createRequest HTTP.Get $ executeReport' settings request
  res <- try $ HTTP.endRequest req
  pure res

execute :: Settings -> Route -> Aff (Either Error HTTP.IncomingResponse)
execute settings (Route.Forward (Forward.Flow entry))  = executeForwardFlow settings entry
execute settings (Route.Report request)                = executeReport settings request
execute settings _                                     = pure $ Left (Exception.error "Invalid route.") 
