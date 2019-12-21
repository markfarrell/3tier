
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
import Effect.Console (log) as Console
import Effect.Class (liftEffect)

import Data.Foldable (fold)
import Data.Traversable(foldMap, sequence)
import Data.Tuple (Tuple(..), snd)
import Data.List (many)
import Data.String.CodeUnits (singleton)

import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.String (string, anyChar)

import DB as DB
import HTTP as HTTP

import Audit as Audit
import Windows as Windows
import Linux as Linux

foreign import decodeURI :: String -> String

foreign import decodeURIComponent :: String -> String

foreign import encodeBase64 :: String -> String

log :: String -> Aff Unit
log = liftEffect <<< Console.log

audit :: Audit.Entry -> HTTP.IncomingMessage -> Aff Unit
audit (Audit.Entry eventType eventID msg) = \req -> do
  result <- try $ DB.runRequest (Audit.insert entry $ req)
  case result of
    (Left error) -> log $ show result
    (Right _)    -> do 
      case entry of
        (Audit.Entry Audit.Failure _ _) -> log $ show entry
        _                               -> pure unit
  where entry = (Audit.Entry eventType eventID (encodeBase64 msg))

data Route = InsertLinux Linux.Entry | InsertWindows Windows.Entry | SummaryWindows | SummaryLinux

instance showRoute :: Show Route where
  show (InsertLinux entry) = "(InsertLinux " <> show entry <> ")"
  show (InsertWindows entry) = "(InsertWindows " <> show entry <> ")"
  show (SummaryWindows) = "(SummaryWindows)"
  show (SummaryLinux) = "(SummaryLinux)"

parseEntryString :: Parser String String
parseEntryString = do
  _ <- string "entry"
  _ <- string "="
  foldMap singleton <$> many anyChar

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

data ContentType a = TextHTML a | TextJSON a

data ResponseType a = Ok (ContentType a) | InternalServerError a | BadRequest String

instance showContentType :: (Show a) => Show (ContentType a) where
  show (TextHTML x) = "TextHTML (" <> show x <> ")"
  show (TextJSON x) = "TextJSON (" <> show x <> ")"

instance showResponseType :: (Show a) => Show (ResponseType a) where
  show (Ok x)                  = "Ok (" <> show x <> ")"
  show (InternalServerError x) = "InternalServerError (" <> show x <> ")"
  show (BadRequest path)       = "BadRequest " <> path

runRoute :: HTTP.IncomingMessage -> Aff (ResponseType String)
runRoute req  = do
  result <-  pure $ flip runParser parseRoute (decodeURIComponent $ HTTP.messageURL req) 
  case result of
    (Left error) -> do 
        _ <- audit (Audit.Entry Audit.Failure Audit.RoutingRequest (show $ Tuple (decodeURIComponent $ HTTP.messageURL req) error)) $ req
        pure $ BadRequest (HTTP.messageURL req)
    (Right (InsertWindows entry)) -> do
      _       <- audit (Audit.Entry Audit.Success Audit.RoutingRequest (show (InsertWindows entry))) $ req
      result' <- DB.runRequest $ Windows.insert entry $ req
      case result' of
        (Left error)             -> do 
           _ <- audit (Audit.Entry Audit.Failure Audit.DatabaseRequest (show error)) $ req 
           pure $ InternalServerError ""
        (Right (Tuple rows steps)) -> do
           _ <- audit (Audit.Entry Audit.Success Audit.DatabaseRequest (show steps)) $ req 
           pure $ Ok (TextHTML "")
    (Right (SummaryWindows)) -> do
      _ <- audit (Audit.Entry Audit.Success Audit.RoutingRequest (show (SummaryWindows))) $ req
      result' <- DB.runRequest $ Windows.summary
      case result' of
        (Left error)             -> do 
           _ <- audit (Audit.Entry Audit.Failure Audit.DatabaseRequest (show error)) $ req 
           pure $ InternalServerError ""
        (Right (Tuple resultSet steps)) -> do
           _ <- audit (Audit.Entry Audit.Success Audit.DatabaseRequest (show steps)) $ req 
           pure $ Ok (TextJSON (show resultSet))
    (Right (InsertLinux entry)) -> do
      _       <- audit (Audit.Entry Audit.Success Audit.RoutingRequest (show (InsertLinux entry))) $ req
      result' <- sequence <$> sequence (DB.runRequest <$> (Linux.insert entry $ req))
      case result' of
        (Left error)             -> do 
           _ <- audit (Audit.Entry Audit.Failure Audit.DatabaseRequest (show error)) $ req 
           pure $ InternalServerError ""
        (Right result'') -> do
           steps <- pure $ fold (snd <$> result'')
           _     <- audit (Audit.Entry Audit.Success Audit.DatabaseRequest (show steps)) $ req 
           pure $ Ok (TextHTML "")
    (Right (SummaryLinux)) -> do
      _ <- audit (Audit.Entry Audit.Success Audit.RoutingRequest (show (SummaryLinux))) $ req
      result' <- DB.runRequest $ Linux.summary
      case result' of
        (Left error)             -> do
           _ <- audit (Audit.Entry Audit.Failure Audit.DatabaseRequest (show error)) $ req
           pure $ InternalServerError ""
        (Right (Tuple resultSet steps)) -> do
           _ <- audit (Audit.Entry Audit.Success Audit.DatabaseRequest (show steps)) $ req
           pure $ Ok (TextJSON (show resultSet))
 
respondResource :: ResponseType String -> HTTP.ServerResponse -> Aff Unit
respondResource (Ok (TextHTML body)) = \res -> liftEffect $ do
  _ <- HTTP.setHeader "Content-Type" "text/html" $ res
  _ <- HTTP.writeHead 200 $ res
  _ <- HTTP.write body $ res
  _ <- HTTP.end $ res
  pure unit
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

producer :: HTTP.Server -> Producer HTTP.Request Aff Unit
producer server = produce \emitter -> do
  HTTP.onRequest (\req res -> emit emitter $ HTTP.Request req res) $ server

consumer :: Consumer HTTP.Request Aff Unit
consumer = forever $ do
  request <- await
  case request of
    (HTTP.Request req res) -> do
      routeResult <- lift $ try (runRoute req)
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
