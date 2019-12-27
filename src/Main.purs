
module Main where 
  
import Prelude

import Control.Alt ((<|>))
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

import Strings as Strings

import DB as DB
import HTTP as HTTP

import Audit as Audit

import Windows as Windows
import Linux as Linux

data Route = InsertLinux Linux.Entry | InsertWindows Windows.Entry | SummaryWindows | SummaryLinux

instance showRoute :: Show Route where
  show (InsertLinux entry) = "(InsertLinux " <> show entry <> ")"
  show (InsertWindows entry) = "(InsertWindows " <> show entry <> ")"
  show (SummaryWindows) = "(SummaryWindows)"
  show (SummaryLinux) = "(SummaryLinux)"

parseInsertWindows :: Parser String Route
parseInsertWindows = do
  _ <- string "/insert/windows"
  _ <- string "?"
  _ <- string "entry"
  _ <- string "="
  entry <- Windows.parseEntry
  pure (InsertWindows entry)

parseSummaryWindows :: Parser String Route
parseSummaryWindows = do
  _ <- string "/summary/windows"
  pure (SummaryWindows)

parseInsertLinux :: Parser String Route
parseInsertLinux = do
  _ <- string "/insert/linux"
  _ <- string "?"
  _ <- string "entry"
  _ <- string "="
  entry <- Linux.parseEntry
  pure (InsertLinux entry)

parseSummaryLinux :: Parser String Route
parseSummaryLinux = do
  _ <- string "/summary/linux"
  pure (SummaryLinux)

parseRoute :: Parser String Route
parseRoute = parseInsertWindows <|> parseSummaryWindows <|> parseInsertLinux <|> parseSummaryLinux

data ContentType a = TextJSON a

data ResponseType a = Ok (ContentType a) | InternalServerError a | BadRequest String

instance showContentType :: (Show a) => Show (ContentType a) where
  show (TextJSON x) = "TextJSON (" <> show x <> ")"

instance showResponseType :: (Show a) => Show (ResponseType a) where
  show (Ok x)                  = "Ok (" <> show x <> ")"
  show (InternalServerError x) = "InternalServerError (" <> show x <> ")"
  show (BadRequest path)       = "BadRequest " <> path

class ContentJSON a where
  showJSON :: a -> String

instance contentJSONUnit :: ContentJSON Unit where
  showJSON _ = ""

instance contentJSONSummary :: ContentJSON (Array (Array String)) where
  showJSON x = show x

runDBRoute :: forall a. ContentJSON a => (HTTP.IncomingMessage -> DB.Request a) -> Route -> HTTP.IncomingMessage -> Aff (ResponseType String)
runDBRoute request route req = do
  _       <- Audit.audit (Audit.Entry Audit.Success Audit.RoutingRequest (show route)) $ req
  result' <- DB.runRequest $ request req
  case result' of
    (Left error)             -> do 
     _ <- Audit.audit (Audit.Entry Audit.Failure Audit.DatabaseRequest (show error)) $ req 
     pure $ InternalServerError ""
    (Right (Tuple result'' steps)) -> do
     _ <- Audit.audit (Audit.Entry Audit.Success Audit.DatabaseRequest (show steps)) $ req 
     pure $ Ok (TextJSON (showJSON result''))

runRoute :: HTTP.IncomingMessage -> Aff (ResponseType String)
runRoute req  = do
  result <-  pure $ flip runParser parseRoute (Strings.decodeURIComponent $ HTTP.messageURL req) 
  case result of
    (Left error) -> do 
        _ <- Audit.audit (Audit.Entry Audit.Failure Audit.RoutingRequest (show $ Tuple (Strings.decodeURIComponent $ HTTP.messageURL req) error)) $ req
        pure $ BadRequest (HTTP.messageURL req)
    (Right (InsertWindows entry)) -> (runDBRoute $ Windows.insert entry)  (InsertWindows entry) $ req
    (Right (SummaryWindows))      -> (runDBRoute $ const Windows.summary) (SummaryWindows)      $ req 
    (Right (InsertLinux entry))   -> (runDBRoute $ Linux.insert entry)    (InsertLinux entry)   $ req 
    (Right (SummaryLinux))        -> (runDBRoute $ const Linux.summary)   (SummaryLinux)        $ req

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
respondResource (InternalServerError _) = \res -> liftEffect $ do
  _ <- HTTP.writeHead 500 $ res
  _ <- HTTP.end $ res
  pure unit

producer :: HTTP.Server -> Producer HTTP.IncomingRequest Aff Unit
producer server = produce \emitter -> do
  HTTP.onRequest (\req res -> emit emitter $ HTTP.IncomingRequest req res) $ server

consumer :: Consumer HTTP.IncomingRequest Aff Unit
consumer = forever $ do
  request <- await
  case request of
    (HTTP.IncomingRequest req res) -> do
      routeResult <- lift $ try (runRoute req)
      case routeResult of
        (Left  error)        -> lift $ Audit.audit (Audit.Entry Audit.Failure Audit.ResourceRequest (show error)) $ req
        (Right responseType) -> do
           _ <- case responseType of
                  (Ok _) -> lift $ Audit.audit (Audit.Entry Audit.Success Audit.ResourceRequest (HTTP.messageURL req)) $ req
                  _      -> lift $ Audit.audit (Audit.Entry Audit.Failure Audit.ResourceRequest (HTTP.messageURL req)) $ req
           responseResult <- lift $ try (respondResource responseType res)
           case responseResult of
             (Left error')   -> lift $ Audit.audit (Audit.Entry Audit.Failure Audit.ResourceResponse (show error')) $ req
             (Right _)       -> lift $ Audit.audit (Audit.Entry Audit.Success Audit.ResourceResponse (show responseType)) $ req 

process :: HTTP.Server -> Process Aff Unit
process server = pullFrom consumer $ producer server

launchProcess :: HTTP.Server -> Effect Unit
launchProcess server = void $ launchAff $ do
  runProcess $ process server

launchServer :: Int -> Effect Unit
launchServer port = do
  server <- HTTP.createServer
  _ <- launchProcess $ server
  _ <- HTTP.listen port $ server
  pure unit

main :: Effect Unit
main = void $ launchServer 3000
