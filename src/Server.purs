module Server 
  ( start
  , main
  ) where 
  
import Prelude

import Control.Alt ((<|>))
import Control.Coroutine (Producer, Consumer, Process, pullFrom, await, runProcess)
import Control.Coroutine.Aff (produce, emit)
import Control.Monad.Error.Class (try)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)

import Data.Either (Either(..))
import Data.Traversable (foldMap, sequence)

import Effect (Effect)
import Effect.Aff (Aff, Fiber, launchAff, forkAff)
import Effect.Class (liftEffect)

import Data.Tuple (Tuple(..))

import Data.String.CodeUnits (singleton)
import Data.List(many)

import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.String (string, anyChar)

import Strings as Strings

import DB as DB
import HTTP as HTTP

import Audit as Audit

import Flow as Flow

import Statistics as Statistics

data Route = ReportStatistics String
  | ForwardFlow Flow.Entry

instance showRoute :: Show Route where
  show (ReportStatistics entry)   = "(ReportStatistics " <> show entry <> ")" 
  show (ForwardFlow entry)      = "(ForwardFlow " <> show entry <> ")"

parseRoute :: Parser String Route
parseRoute = forwardFlow <|> reportStatistics 
  where
    forwardFlow = do
      _     <- string "/forward/flow?q="
      entry <- Flow.parseEntry
      pure (ForwardFlow entry)
    reportStatistics = do
      _     <- string "/report/statistics?q="
      table <- foldMap singleton <$> many anyChar
      pure (ReportStatistics table)

data ContentType a = TextJSON a | TextHTML a

data AuthenticationType = Bearer

data ResponseType a = Ok (ContentType a) | InternalServerError a | BadRequest String | Forbidden AuthenticationType String

instance showContentType :: (Show a) => Show (ContentType a) where
  show (TextJSON x) = "(TextJSON (" <> show x <> "))"
  show (TextHTML x) = "(TextHTML (" <> show x <> "))" 

instance showAuthenticationType :: Show AuthenticationType where
  show Bearer = "Bearer"

instance showResponseType :: (Show a) => Show (ResponseType a) where
  show (Ok x)                  = "Ok (" <> show x <> ")"
  show (InternalServerError x) = "InternalServerError (" <> show x <> ")"
  show (BadRequest path)       = "BadRequest " <> path
  show (Forbidden ty realm)    = "Forbidden " <> show ty <> " " <> realm

class ContentJSON a where
  showJSON :: a -> String

instance contentJSONUnit :: ContentJSON Unit where
  showJSON _ = ""

instance contentJSONString :: ContentJSON String where
  showJSON x = x

runRequest' :: forall a. ContentJSON a => String -> (HTTP.IncomingMessage -> DB.Request a) -> HTTP.IncomingMessage -> Aff (ResponseType String)
runRequest' filename request req = do
  result' <- DB.runRequest $ request req
  case result' of
    (Left error)             -> do 
     _ <- audit (Audit.Entry Audit.Failure Audit.DatabaseRequest (show error)) $ req 
     pure $ InternalServerError ""
    (Right (Tuple result'' steps)) -> do
     _ <- audit (Audit.Entry Audit.Success Audit.DatabaseRequest (show steps)) $ req 
     pure $ Ok (TextJSON (showJSON result''))
  where audit = Audit.application filename

runRoute :: String -> HTTP.IncomingMessage -> Aff (ResponseType String)
runRoute filename req  = do
  result <-  pure $ flip runParser parseRoute $ Strings.decodeURIComponent (HTTP.messageURL req)
  case result of
    (Left error) -> do 
      _ <- audit (Audit.Entry Audit.Failure Audit.RoutingRequest (show error)) $ req
      pure $ BadRequest (HTTP.messageURL req)
    (Right route) -> do
      _       <- audit (Audit.Entry Audit.Success Audit.RoutingRequest (show route)) $ req
      case route of 
        (ForwardFlow entry)        -> (runRequest' filename $ insertFlow entry)        $ req
        (ReportStatistics table)     -> (runRequest' filename $ reportStatistics table)    $ req
  where
    insertFlow      = Flow.insert filename
    reportStatistics  = const <<< Statistics.report filename
    audit             = Audit.application filename

respondResource :: ResponseType String -> HTTP.ServerResponse -> Aff Unit
respondResource (Ok (TextJSON body)) = \res -> liftEffect $ do
  _ <- HTTP.setHeader "Content-Type" "text/json" $ res
  _ <- HTTP.writeHead 200 $ res
  _ <- HTTP.write body $ res
  _ <- HTTP.end $ res
  pure unit
respondResource (Ok (TextHTML body)) = \res -> liftEffect $ do
  _ <- HTTP.setHeader "Content-Type" "text/html" $ res
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

consumer :: String -> Consumer HTTP.IncomingRequest Aff Unit
consumer filename = forever $ do
  request <- await
  case request of
    (HTTP.IncomingRequest req res) -> do
      routeResult <- lift $ try (runRoute' req)
      case routeResult of
        (Left  error)        -> lift $ audit (Audit.Entry Audit.Failure Audit.ResourceRequest (show error)) $ req
        (Right responseType) -> do
           _ <- case responseType of
                  (Ok _) -> lift $ audit (Audit.Entry Audit.Success Audit.ResourceRequest (HTTP.messageURL req)) $ req
                  _      -> lift $ audit (Audit.Entry Audit.Failure Audit.ResourceRequest (HTTP.messageURL req)) $ req
           responseResult <- lift $ try (respondResource responseType res)
           case responseResult of
             (Left error')   -> lift $ audit (Audit.Entry Audit.Failure Audit.ResourceResponse (show error')) $ req
             (Right _)       -> lift $ audit (Audit.Entry Audit.Success Audit.ResourceResponse (show responseType)) $ req 
  where 
    runRoute' = runRoute filename
    audit     = Audit.application filename

process :: String -> HTTP.Server -> Process Aff Unit
process filename server = pullFrom (consumer filename) (producer server)

initialize :: DB.Request String
initialize = do
  filename <- lift $ getFilename
  _        <- DB.touch filename
  _        <- sequence $ schemas filename
  pure filename
  where
    schemas filename =
      [ Audit.schema filename
      , Flow.schema filename
      ]
    getFilename = pure "logs.db"

start :: HTTP.Server -> Aff (Fiber Unit)
start server = do
  result <- DB.runRequest initialize
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
