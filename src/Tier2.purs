module Tier2 
  ( start
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

import Data.Tuple (Tuple(..))

import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.String (string)
import Text.Parsing.Parser.Combinators (choice)

import Unsafe.Coerce (unsafeCoerce)

import Strings as Strings
import JSON as JSON

import Tier3 as Tier3

import Date as Date
import HTTP as HTTP

import Audit as Audit
import Flow as Flow
import Report as Report

data Route = Forward Tier3.Query | Report Tier3.Query

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

reportAudit'' :: Audit.EventID -> String
reportAudit'' Audit.DatabaseRequest = "database-request"
reportAudit'' Audit.ResourceRequest = "resource-request"
reportAudit'' Audit.RoutingRequest  = "routing-request"

reportAudit' :: Audit.EventID -> Parser String Route
reportAudit' eventID = choice [w, x, y, z] 
  where
    w = do
      _ <- string ("/report/audit/" <> reportAudit'' eventID <> "/success/sources")
      pure (Report (Tier3.SelectQuery $ Report.Audit eventID Audit.Success Report.Sources))
    x = do
      _ <- string ("/report/audit/" <> reportAudit'' eventID <> "/failure/sources")
      pure (Report (Tier3.SelectQuery $ Report.Audit eventID Audit.Failure Report.Sources))
    y = do
      _ <- string ("/report/audit/" <> reportAudit'' eventID <> "/success/durations")
      pure (Report (Tier3.SelectQuery $ Report.Audit eventID Audit.Success Report.Durations))
    z = do
      _ <- string ("/report/audit/" <> reportAudit'' eventID <> "/failure/durations")
      pure (Report (Tier3.SelectQuery $ Report.Audit eventID Audit.Failure Report.Durations))

reportAudit :: Parser String Route
reportAudit = choice $ reportAudit' <$> [Audit.DatabaseRequest, Audit.ResourceRequest, Audit.RoutingRequest] 

report :: Parser String Route
report = choice [reportAudit]

forwardFlow :: Parser String Route
forwardFlow = do
  _     <- string "/forward/flow?q="
  entry <- Flow.parse
  pure (Forward (Tier3.InsertQuery $ Tier3.InsertFlow entry))

forward :: Parser String Route
forward = choice [forwardFlow] 

route :: Parser String Route
route = choice [forward, report]

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
    (Left _)  -> pure  $ InternalServerError ""
    (Right _) -> pure  $ Ok (TextJSON "")

databaseRequest :: Tier3.Settings -> Route -> HTTP.IncomingMessage -> Aff (Resource String)
databaseRequest settings (Forward query) req = databaseRequest' settings query req
databaseRequest settings (Report query) req  = databaseRequest' settings query req

routingRequest' :: forall a. Either a Route -> Number -> Audit.Entry
routingRequest' (Left _)                                                    = \duration -> Audit.Entry Audit.Failure Audit.RoutingRequest duration "???"               
routingRequest' (Right (Forward (Tier3.InsertQuery (Tier3.InsertFlow _))))  = \duration -> Audit.Entry Audit.Success Audit.RoutingRequest duration "FORWARD-FLOW"
routingRequest' (Right (Forward _))                                         = \duration -> Audit.Entry Audit.Success Audit.RoutingRequest duration "???"
routingRequest' (Right (Report (Tier3.SelectQuery (Report.Audit _ _ _))))   = \duration -> Audit.Entry Audit.Success Audit.RoutingRequest duration "REPORT-AUDIT"
routingRequest' (Right (Report _))                                          = \duration -> Audit.Entry Audit.Success Audit.RoutingRequest duration "???"

routingRequest :: Tier3.Settings -> HTTP.IncomingMessage -> Aff (Either HTTP.IncomingMessage Route)
routingRequest settings req = do 
  startTime <- liftEffect $ Date.currentTime
  result    <- pure $ flip runParser route $ Strings.decodeURIComponent (HTTP.messageURL req)
  endTime   <- liftEffect $ Date.currentTime
  duration  <- pure $ endTime - startTime
  entry     <- pure $ routingRequest' result duration
  _         <- audit settings entry req 
  case result of
    (Left _)       -> pure $ Left req
    (Right route') -> pure $ Right route'

resourceRequest''' :: forall a b. Either a (Resource b) -> Number -> Audit.Entry
resourceRequest''' (Left _)                         = \duration -> Audit.Entry Audit.Failure Audit.ResourceRequest duration  "???"
resourceRequest''' (Right (Ok _))                   = \duration -> Audit.Entry Audit.Success Audit.ResourceRequest duration "200-OK" 
resourceRequest''' (Right (InternalServerError _))  = \duration -> Audit.Entry Audit.Failure Audit.ResourceRequest duration "500-INTERNAL-SERVER-ERROR"
resourceRequest''' (Right (BadRequest _))           = \duration -> Audit.Entry Audit.Failure Audit.ResourceRequest duration "400-BAD-REQUEST"
resourceRequest''' (Right (Forbidden _ _))          = \duration -> Audit.Entry Audit.Failure Audit.ResourceRequest duration "403-FORBIDDEN"

resourceRequest'' :: Tier3.Settings -> HTTP.IncomingMessage -> Aff (Resource String)
resourceRequest'' settings req  = do
  result <- routingRequest settings req
  case result of
    (Left req')    -> pure $ BadRequest (HTTP.messageURL req')
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

main :: Effect Unit
main = do
  server <- HTTP.createServer
  _ <- void $ launchAff $ start settings server
  _ <- HTTP.listen port server
  pure unit
  where 
    port = 3000
    settings = "Tier2.db"
