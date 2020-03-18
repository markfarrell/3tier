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

audit :: Tier3.Settings -> Audit.Entry -> HTTP.IncomingMessage -> Aff Unit
audit filename entry req = do
  _ <- Tier3.execute $ Tier3.insert filename (Tier3.InsertAudit entry) req
  pure unit

parseRoute :: Parser String Route
parseRoute = forward
  where
    forward = do
      _     <- string "/forward/flow?q="
      entry <- Flow.parse
      pure (Forward (Tier3.InsertFlow entry))

data ContentType a = TextJSON a

data AuthenticationType = Bearer

data ResponseType a = Ok (ContentType a) | InternalServerError a | BadRequest String | Forbidden AuthenticationType String

instance showContentType :: (Show a) => Show (ContentType a) where
  show (TextJSON x) = "(TextJSON (" <> show x <> "))"

instance showAuthenticationType :: Show AuthenticationType where
  show Bearer = "Bearer"

instance showResponseType :: (Show a) => Show (ResponseType a) where
  show (Ok x)                  = "Ok (" <> show x <> ")"
  show (InternalServerError x) = "InternalServerError (" <> show x <> ")"
  show (BadRequest path)       = "BadRequest " <> path
  show (Forbidden ty realm)    = "Forbidden " <> show ty <> " " <> realm

class ContentJSON a where
  showJSON :: a -> String

instance contentJSONUnit :: ContentJSON Tier3.ResultSet where
  showJSON (Tier3.InsertResult _) = ""
  showJSON (Tier3.SelectResult x) = JSON.stringify $ unsafeCoerce x

instance contentJSONString :: ContentJSON String where
  showJSON x = x

runRequest' :: forall a. ContentJSON a => Tier3.Settings -> (HTTP.IncomingMessage -> Tier3.Request a) -> HTTP.IncomingMessage -> Aff (ResponseType String)
runRequest' filename request req = do
  startTime   <- liftEffect $ Date.currentTime
  result'     <- Tier3.execute $ request req
  endTime     <- liftEffect $ Date.currentTime
  duration    <- pure $ endTime - startTime
  case result' of
    (Left error)             -> do 
      _ <- audit filename (Audit.Entry Audit.Failure Audit.DatabaseRequest duration (audit'' result')) $ req
      pure $ InternalServerError ""
    (Right (Tuple result'' steps)) -> do
      _ <- audit filename (Audit.Entry Audit.Success Audit.DatabaseRequest duration (audit'' result')) $ req
      pure $ Ok (TextJSON (showJSON result''))
  where 
    audit'' (Left _)      = "" 
    audit'' (Right (Tuple _ steps)) = show steps

runRoute :: Tier3.Settings -> HTTP.IncomingMessage -> Aff (ResponseType String)
runRoute filename req  = do
  startTime <- liftEffect $ Date.currentTime
  result    <- pure $ flip runParser parseRoute $ Strings.decodeURIComponent (HTTP.messageURL req)
  endTime   <- liftEffect $ Date.currentTime
  duration  <- pure $ endTime - startTime
  case result of
    (Left _     ) -> do 
      _ <- audit filename (Audit.Entry Audit.Failure Audit.RoutingRequest duration (route' result)) $ req
      pure $ BadRequest (HTTP.messageURL req)
    (Right route) -> do
      _ <- audit filename (Audit.Entry Audit.Success Audit.RoutingRequest duration (route' result)) $ req
      case route of 
        (Forward insertQuery) -> (runRequest' filename $ Tier3.insert filename insertQuery) $ req
  where
    route' (Left _)                               = "ANY"               
    route' (Right (Forward (Tier3.InsertFlow _)))    = "FORWARD-FLOW"
    route' (Right (Forward (Tier3.InsertAudit _)))   = "FORWARD-AUDIT"

respondResource :: ResponseType String -> HTTP.ServerResponse -> Aff Unit
respondResource (Ok (TextJSON body)) = \res -> liftEffect $ do
  _ <- HTTP.setHeader "Content-Type" "text/json" $ res
  _ <- HTTP.writeHead 200 $ res
  _ <- HTTP.write body $ res
  _ <- HTTP.end $ res
  pure unit
respondResource (BadRequest _) = \res -> liftEffect $ do
  _ <- HTTP.writeHead 400 $ res
  _ <- HTTP.end $ res
  pure unit  
respondResource (Forbidden Bearer realm) = \res -> liftEffect $ do
  _ <- HTTP.setHeader "WWW-Authenticate" ("Bearer realm=" <> realm) $ res
  _ <- HTTP.writeHead 401 $ res
  _ <- HTTP.end $ res
  pure unit  
respondResource (InternalServerError _) = \res -> liftEffect $ do
  _ <- HTTP.writeHead 500 $ res
  _ <- HTTP.end $ res
  pure unit

producer :: HTTP.Server -> Producer HTTP.IncomingRequest Aff Unit
producer server = produce \emitter -> do
  HTTP.onRequest (\req res -> emit emitter $ HTTP.IncomingRequest req res) $ server

consumer :: Tier3.Settings -> Consumer HTTP.IncomingRequest Aff Unit
consumer filename = forever $ do
  request <- await
  case request of
    (HTTP.IncomingRequest req res) -> do
      startTime   <- lift $ liftEffect $ Date.currentTime
      routeResult <- lift $ try (runRoute' req)
      endTime     <- lift $ liftEffect $ Date.currentTime
      duration    <- lift $ pure (endTime - startTime)
      case routeResult of
        (Left  error)        -> lift $ audit filename (Audit.Entry Audit.Failure Audit.ResourceRequest duration default) $ req
        (Right ty) -> do
           _ <- case ty of
                  (Ok _) -> lift $ audit filename (Audit.Entry Audit.Success Audit.ResourceRequest duration default) $ req
                  _      -> lift $ audit filename (Audit.Entry Audit.Failure Audit.ResourceRequest duration default) $ req
           startTime'     <- lift $ liftEffect $ Date.currentTime
           responseResult <- lift $ try (respondResource ty res)
           endTime'       <- lift $ liftEffect $ Date.currentTime
           duration'      <- lift $ pure (endTime' - startTime')
           case responseResult of
             (Left  _)   -> lift $ audit filename (Audit.Entry Audit.Failure Audit.ResourceResponse duration' default)   $ req
             (Right _)   -> lift $ audit filename (Audit.Entry Audit.Success Audit.ResourceResponse duration' (responseType ty)) $ req 
  where 
    runRoute' = runRoute filename
    default   = ""
    responseType (Ok _)                  = "OK"
    responseType (BadRequest _)          = "BAD-REQUEST"
    responseType (Forbidden _ _)         = "FORBIDDEN"
    responseType (InternalServerError _) = "INTERNAL-SERVER-ERROR"

process :: Tier3.Settings -> HTTP.Server -> Process Aff Unit
process filename server = pullFrom (consumer filename) (producer server)

initialize :: Tier3.Request Tier3.Settings
initialize = do
  filename <- lift $ getFilename
  _        <- Tier3.touch filename
  _        <- sequence $ schemas filename
  pure filename
  where
    schemas filename =
      [ Tier3.schema Tier3.Audit $ filename 
      , Tier3.schema Tier3.Flow  $ filename
      ]
    getFilename = pure $ UUIDv1.defaultUUID <> ".db"

start :: HTTP.Server -> Aff (Fiber Unit)
start server = do
  result <- Tier3.execute initialize
  case result of
    (Left error)                -> forkAff $ pure unit
    (Right (Tuple filename _))  -> forkAff $ start' filename
  where 
    start' filename = do
      runProcess $ process filename server

main :: Effect Unit
main = do
  server <- HTTP.createServer
  _ <- void $ launchAff $ start server
  _ <- HTTP.listen port server
  pure unit
  where port = 3000
