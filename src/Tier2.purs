module Tier2 
  ( Settings(..)
  , start
  , forwardFlow
  , main
  ) where 
  
import Prelude

import Control.Coroutine (Producer, Consumer, Process, pullFrom, await, runProcess)
import Control.Coroutine.Aff (produce, emit)
import Control.Monad.Error.Class (try)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)

import Data.Either (Either(..))

import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)

import Data.Tuple (Tuple(..))

import Unsafe.Coerce (unsafeCoerce)

import JSON as JSON
import Strings as Strings

import Tier3 as Tier3

import Date as Date
import HTTP as HTTP

import Audit as Audit
import Flow as Flow

import Tier2.Route (Route(..))
import Tier2.Route as Route

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

audit :: Tier3.Settings -> Audit.Entry -> HTTP.IncomingMessage -> Aff Unit
audit settings entry req = do
  _ <- Tier3.execute $ Tier3.request settings (Tier3.InsertQuery $ Tier3.InsertAudit entry) req
  pure unit

databaseRequest'' :: forall a. Tier3.Result a -> Number -> Audit.Entry
databaseRequest'' (Left _)                = \duration -> Audit.Entry Audit.Failure Audit.DatabaseRequest duration "???"
databaseRequest'' (Right (Tuple _ steps)) = \duration -> Audit.Entry Audit.Success Audit.DatabaseRequest duration (show steps)

databaseRequest':: Tier3.Settings -> Tier3.Query -> HTTP.IncomingMessage -> Aff (Resource String) 
databaseRequest' settings query req = do
  startTime <- liftEffect $ Date.currentTime
  result    <- Tier3.execute $ Tier3.request settings query req
  endTime   <- liftEffect $ Date.currentTime
  duration  <- pure $ endTime - startTime
  entry     <- pure $ databaseRequest'' result duration
  _         <- audit settings entry req 
  case result of 
    (Left _)                    -> pure  $ InternalServerError ""
    (Right (Tuple resultSet _)) -> pure  $ Ok (TextJSON (showJSON resultSet))

databaseRequest :: Tier3.Settings -> Route -> HTTP.IncomingMessage -> Aff (Resource String)
databaseRequest settings (Forward query) req = databaseRequest' settings query req
databaseRequest settings (Report query) req  = databaseRequest' settings query req

resourceRequest''' :: forall a b. Either a (Resource b) -> Number -> Audit.Entry
resourceRequest''' (Left _)                         = \duration -> Audit.Entry Audit.Failure Audit.ResourceRequest duration  "???"
resourceRequest''' (Right (Ok _))                   = \duration -> Audit.Entry Audit.Success Audit.ResourceRequest duration "200-OK" 
resourceRequest''' (Right (InternalServerError _))  = \duration -> Audit.Entry Audit.Failure Audit.ResourceRequest duration "500-INTERNAL-SERVER-ERROR"
resourceRequest''' (Right (BadRequest _))           = \duration -> Audit.Entry Audit.Failure Audit.ResourceRequest duration "400-BAD-REQUEST"
resourceRequest''' (Right (Forbidden _ _))          = \duration -> Audit.Entry Audit.Failure Audit.ResourceRequest duration "403-FORBIDDEN"

resourceRequest'' :: Tier3.Settings -> HTTP.IncomingMessage -> Aff (Resource String)
resourceRequest'' settings req  = do
  result <- Route.execute settings req
  case result of
    (Left  _)      -> pure $ BadRequest (HTTP.messageURL req)
    (Right route') -> databaseRequest settings route' req

resourceRequest' :: Tier3.Settings -> HTTP.IncomingRequest -> Aff (Resource String)
resourceRequest' settings (HTTP.IncomingRequest req res) = do
  response <- resourceRequest'' settings req
  _        <- sendResource response res
  pure response

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
  result          <- try $ resourceRequest' settings $ HTTP.IncomingRequest req res
  endTime         <- liftEffect $ Date.currentTime
  duration        <- pure (endTime - startTime)
  entry           <- pure $ resourceRequest''' result duration 
  _               <- audit settings entry req
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

start :: Tier3.Settings -> HTTP.Server -> Aff Unit
start settings server = runProcess $ process settings server

forwardFlow :: Settings -> Flow.Entry -> Aff (Either Error HTTP.IncomingResponse)
forwardFlow (Settings settings) = \entry -> do
  result <- pure $ Flow.write entry
  case result of
    (Left error)       -> pure (Left error)
    (Right identifier) -> do
      req <- HTTP.createRequest HTTP.Post $ url identifier
      res <- try $ HTTP.endRequest req
      pure res
  where 
    url identifier = location <> forwardRoute <> "/" <> Strings.encodeURIComponent identifier
    location      = "http://" <> settings.host <> ":" <> show settings.port
    forwardRoute  = "/forward/flow"

main :: Effect Unit
main = do
  server <- HTTP.createServer
  _ <- void $ launchAff $ start settings server
  _ <- HTTP.listen port server
  pure unit
  where 
    port = 3000
    settings = "Tier2.db"
