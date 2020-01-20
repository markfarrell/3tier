module SIEM.Logging.Server 
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
import Data.Traversable (sequence)

import Effect (Effect)
import Effect.Aff (Aff, Fiber, launchAff, forkAff)
import Effect.Class (liftEffect)

import Data.Tuple (Tuple(..))

import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.String (string)

import Strings as Strings

import DB as DB
import HTTP as HTTP

import Audit as Audit

import SIEM.Logging.Linux as Linux
import SIEM.Logging.Sensor as Sensor
import SIEM.Logging.Windows as Windows

import SIEM.Logging.Session as Session
import SIEM.Logging.Statistics as Statistics

data Route = CreateLogID
  | ReportStatistics
  | ForwardLinux Linux.Entry 
  | ForwardWindows Windows.Entry 
  | ForwardSensor Sensor.Entry

instance showRoute :: Show Route where
  show (CreateLogID)          = "(CreateLogID)"
  show (ReportStatistics)     = "(ReportStatistics)"
  show (ForwardLinux entry)   = "(ForwardLinux " <> show entry <> ")"
  show (ForwardWindows entry) = "(ForwardWindows " <> show entry <> ")"
  show (ForwardSensor entry)  = "(ForwardSensor " <> show entry <> ")"

parseRoute :: Parser String Route
parseRoute = createLogID <|> reportStatistics <|> forwardLinux <|> forwardWindows <|> forwardSensor
  where
    createLogID = do
      _ <- string "/create/log-id"
      pure (CreateLogID)
    reportStatistics = do
      _     <- string "/report/statistics"
      pure (ReportStatistics)
    forwardLinux = do
      _     <- string "/forward/linux?entry="
      entry <- Linux.parseEntry
      pure (ForwardLinux entry)
    forwardWindows = do
      _     <- string "/forward/windows?entry="
      entry <- Windows.parseEntry
      pure (ForwardWindows entry)
    forwardSensor = do
      _     <- string "/forward/sensor?entry="
      entry <- Sensor.parseEntry
      pure (ForwardSensor entry)

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

runCreateLogID :: String -> HTTP.IncomingMessage -> Aff (ResponseType String)
runCreateLogID filename req = do
  logID <- Session.createLogID
  pure $ Ok (TextHTML logID)

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
        (ForwardWindows entry) -> (runRequest' filename $ insertWindows entry)       $ req
        (ForwardLinux entry)   -> (runRequest' filename $ insertLinux entry)         $ req 
        (ForwardSensor entry)  -> (runRequest' filename $ insertSensor entry)        $ req
        (ReportStatistics)     -> (runRequest' filename $ const reportStatistics)    $ req
        (CreateLogID)          -> (runCreateLogID filename)                          $ req
  where
    insertWindows    = Windows.insert filename
    insertLinux      = Linux.insert filename
    insertSensor     = Sensor.insert filename
    reportStatistics = Statistics.report filename
    audit            = Audit.application filename

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
  _       <- lift $ Session.echoLogID request
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
      , Windows.schema filename
      , Sensor.schema filename
      , Linux.schema filename
      ]
    getFilename = pure "logs.db"

start :: HTTP.Server -> Aff (Fiber Unit)
start server = do
  result <- DB.runRequest initialize
  case result of
    (Left error) -> do
       _ <- audit' (Audit.Entry Audit.Failure Audit.DatabaseRequest (show error))
       forkAff $ pure unit
    (Right (Tuple filename steps)) -> do
       _ <- audit' (Audit.Entry Audit.Success Audit.DatabaseRequest (show steps))
       forkAff $ start' filename
  where 
    start' filename = do
      runProcess $ process filename server
    audit' = Audit.debug

main :: Effect Unit
main = do
  server <- HTTP.createServer
  _ <- void $ launchAff $ start server
  _ <- HTTP.listen port server
  pure unit
  where port = 3000
