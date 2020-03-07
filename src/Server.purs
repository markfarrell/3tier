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

import Date as Date
import HTTP as HTTP
import UUIDv1 as UUIDv1

import Audit as Audit
import Flow as Flow
import Statistics as Statistics

data ForwardType = Flow Flow.Entry

data ReportType = Statistics DB.Table

data Route = Forward ForwardType | Report ReportType

instance showRoute :: Show Route where
  show (Report (Statistics table)) = "(Report (Statistics " <> show table <> "))" 
  show (Forward (Flow entry))      = "(Forward (Flow " <> show entry <> "))"

parseRoute :: Parser String Route
parseRoute = forward <|> report
  where
    forward = do
      _     <- string "/forward/flow?q="
      entry <- Flow.parse
      pure (Forward (Flow entry))
    report  = do
      _     <- string "/report/statistics?q="
      table <- foldMap singleton <$> many anyChar
      pure (Report (Statistics table))

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

instance contentJSONUnit :: ContentJSON Unit where
  showJSON _ = ""

instance contentJSONString :: ContentJSON String where
  showJSON x = x

now :: Aff Number
now = Date.getMilliseconds <$> (liftEffect $ Date.current)

runRequest' :: forall a. ContentJSON a => DB.Database -> (HTTP.IncomingMessage -> DB.Request a) -> HTTP.IncomingMessage -> Aff (ResponseType String)
runRequest' filename request req = do
  startTime   <- now
  result'     <- DB.runRequest $ request req
  endTime     <- now
  duration    <- pure $ endTime - startTime
  case result' of
    (Left error)             -> do 
     _     <- audit (Audit.Entry Audit.Failure Audit.DatabaseRequest duration (show error)) $ req 
     pure $ InternalServerError ""
    (Right (Tuple result'' steps)) -> do
     _ <- audit (Audit.Entry Audit.Success Audit.DatabaseRequest duration (show steps)) $ req 
     pure $ Ok (TextJSON (showJSON result''))
  where 
    audit                 = Audit.application filename
    audit' x y z          = audit $ Audit.Entry x Audit.DatabaseRequest y (audit'' z)
    audit'' (Left _)      = "" 
    audit'' (Right steps) = steps

runRoute :: DB.Database -> HTTP.IncomingMessage -> Aff (ResponseType String)
runRoute filename req  = do
  startTime <- now
  result    <- pure $ flip runParser parseRoute $ Strings.decodeURIComponent (HTTP.messageURL req)
  endTime   <- now
  duration  <- pure $ endTime - startTime
  case result of
    (Left _     ) -> do 
      _ <- audit' Audit.Failure duration result $ req
      pure $ BadRequest (HTTP.messageURL req)
    (Right route) -> do
      _ <- audit' Audit.Success duration result $ req
      case route of 
        (Forward (Flow entry))          -> (runRequest' filename $ insertFlow entry)        $ req
        (Report (Statistics table))     -> (runRequest' filename $ reportStatistics table)  $ req
  where
    insertFlow        = Flow.insert filename
    reportStatistics  = const <<< Statistics.report filename
    audit             = Audit.application filename
    audit' x y z      = audit $ Audit.Entry x Audit.RoutingRequest y (audit'' z)
    audit'' (Left _)                         = ""
    audit'' (Right (Forward (Flow _)))       = "FORWARD-FLOW"
    audit'' (Right (Report  (Statistics _))) = "REPORT-STATISTICS"

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

consumer :: DB.Database -> Consumer HTTP.IncomingRequest Aff Unit
consumer filename = forever $ do
  request <- await
  case request of
    (HTTP.IncomingRequest req res) -> do
      startTime   <- lift $ now
      routeResult <- lift $ try (runRoute' req)
      endTime     <- lift $ now
      duration    <- lift $ pure (endTime - startTime)
      case routeResult of
        (Left  error)        -> lift $ audit (Audit.Entry Audit.Failure Audit.ResourceRequest duration default) $ req
        (Right ty) -> do
           _ <- case ty of
                  (Ok _) -> lift $ audit (Audit.Entry Audit.Success Audit.ResourceRequest duration default) $ req
                  _      -> lift $ audit (Audit.Entry Audit.Failure Audit.ResourceRequest duration default) $ req
           startTime'     <- lift $ now
           responseResult <- lift $ try (respondResource ty res)
           endTime'       <- lift $ now
           duration'      <- lift $ pure (endTime' - startTime')
           case responseResult of
             (Left _     )   -> lift $ audit (Audit.Entry Audit.Failure Audit.ResourceResponse duration' default)   $ req
             (Right _)       -> lift $ audit (Audit.Entry Audit.Success Audit.ResourceResponse duration' (responseType ty)) $ req 
  where 
    runRoute' = runRoute filename
    audit     = Audit.application filename
    default   = ""
    responseType (Ok _)                  = "OK"
    responseType (BadRequest _)          = "BAD-REQUEST"
    responseType (Forbidden _ _)         = "FORBIDDEN"
    responseType (InternalServerError _) = "INTERNAL-SERVER-ERROR"

process :: DB.Database -> HTTP.Server -> Process Aff Unit
process filename server = pullFrom (consumer filename) (producer server)

initialize :: DB.Request DB.Database
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
    getFilename = pure $ UUIDv1.defaultUUID <> ".db"

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
