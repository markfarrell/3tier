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
import Data.Traversable (sequence)

import Effect (Effect)
import Effect.Aff (Aff, Fiber, launchAff, forkAff)
import Effect.Class (liftEffect)

import Data.Tuple (Tuple(..))

import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.String (string)

import Unsafe.Coerce (unsafeCoerce)

import Strings as Strings
import JSON as JSON

import Tier3 as Tier3

import Date as Date
import HTTP as HTTP
import UUIDv1 as UUIDv1

import Audit as Audit
import Flow as Flow

data Route = Forward Tier3.Insert

data ContentType a = TextJSON a

data AuthenticationType = Bearer

data Resource a = Ok (ContentType a) | InternalServerError a | BadRequest String | Forbidden AuthenticationType String

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
  _ <- Tier3.execute $ Tier3.insert settings (Tier3.InsertAudit entry) req
  pure unit


parseRoute :: Parser String Route
parseRoute = forward
  where
    forward = do
      _     <- string "/forward/flow?q="
      entry <- Flow.parse
      pure (Forward (Tier3.InsertFlow entry))

runRequest' :: forall a. ContentJSON a => Tier3.Settings -> (HTTP.IncomingMessage -> Tier3.Request a) -> HTTP.IncomingMessage -> Aff (Resource String)
runRequest' settings request req = do
  startTime   <- liftEffect $ Date.currentTime
  result'     <- Tier3.execute $ request req
  endTime     <- liftEffect $ Date.currentTime
  duration    <- pure $ endTime - startTime
  case result' of
    (Left error)             -> do 
      _ <- audit settings (Audit.Entry Audit.Failure Audit.DatabaseRequest duration (audit'' result')) $ req
      pure $ InternalServerError ""
    (Right (Tuple result'' steps)) -> do
      _ <- audit settings (Audit.Entry Audit.Success Audit.DatabaseRequest duration (audit'' result')) $ req
      pure $ Ok (TextJSON (showJSON result''))
  where 
    audit'' (Left _)      = "" 
    audit'' (Right (Tuple _ steps)) = show steps

resourceRequest''' :: forall a b. Either a (Resource b) -> Number -> Audit.Entry
resourceRequest''' (Left _)                         = \duration -> Audit.Entry Audit.Failure Audit.ResourceRequest duration  ""
resourceRequest''' (Right (Ok _))                   = \duration -> Audit.Entry Audit.Success Audit.ResourceRequest duration "200-OK" 
resourceRequest''' (Right (InternalServerError _))  = \duration -> Audit.Entry Audit.Failure Audit.ResourceRequest duration "500-INTERNAL-SERVER-ERROR"
resourceRequest''' (Right (BadRequest _))           = \duration -> Audit.Entry Audit.Failure Audit.ResourceRequest duration "400-BAD-REQUEST"
resourceRequest''' (Right (Forbidden _ _))          = \duration -> Audit.Entry Audit.Failure Audit.ResourceRequest duration "403-FORBIDDEN"

resourceRequest'' :: Tier3.Settings -> HTTP.IncomingMessage -> Aff (Resource String)
resourceRequest'' settings req  = do
  startTime <- liftEffect $ Date.currentTime
  result    <- pure $ flip runParser parseRoute $ Strings.decodeURIComponent (HTTP.messageURL req)
  endTime   <- liftEffect $ Date.currentTime
  duration  <- pure $ endTime - startTime
  case result of
    (Left _     ) -> do 
      _ <- audit settings (Audit.Entry Audit.Failure Audit.RoutingRequest duration (route' result)) $ req
      pure $ BadRequest (HTTP.messageURL req)
    (Right route) -> do
      _ <- audit settings (Audit.Entry Audit.Success Audit.RoutingRequest duration (route' result)) $ req
      case route of 
        (Forward insertQuery) -> (runRequest' settings $ Tier3.insert settings insertQuery) $ req
  where
    route' (Left _)                               = "ANY"               
    route' (Right (Forward (Tier3.InsertFlow _)))    = "FORWARD-FLOW"
    route' (Right (Forward (Tier3.InsertAudit _)))   = "FORWARD-AUDIT"

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

initialize :: Tier3.Request Tier3.Settings
initialize = do
  settings <- lift $ getFilename
  _        <- Tier3.touch settings
  _        <- sequence $ schemas settings
  pure settings
  where
    schemas settings =
      [ Tier3.schema Tier3.Audit $ settings 
      , Tier3.schema Tier3.Flow  $ settings
      ]
    getFilename = pure $ UUIDv1.defaultUUID <> ".db"

start :: HTTP.Server -> Aff (Fiber Unit)
start server = do
  result <- Tier3.execute initialize
  case result of
    (Left error)                -> forkAff $ pure unit
    (Right (Tuple settings _))  -> forkAff $ start' settings
  where 
    start' settings = do
      runProcess $ process settings server

main :: Effect Unit
main = do
  server <- HTTP.createServer
  _ <- void $ launchAff $ start server
  _ <- HTTP.listen port server
  pure unit
  where port = 3000
