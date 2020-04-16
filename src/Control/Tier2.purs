module Control.Tier2 
  ( Settings(..)
  , Authentication(..)
  , Authorization(..)
  , Server(..)
  , Role(..)
  , URI(..)
  , Query
  , Request
  , Resource
  , Result
  , request
  , execute
  , process
  ) where 
  
import Prelude

import Control.Coroutine (Producer, Consumer, Process, pullFrom, await)
import Control.Coroutine.Aff (produce, emit)
import Control.Monad.Error.Class (try)
import Control.Monad.Free.Trans (liftFreeT, runFreeT)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)

import Data.Either (Either(..))

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw) as Exception

import Text.Parsing.Parser (runParser)

import Unsafe.Coerce (unsafeCoerce)

import FFI.Date as Date
import FFI.HTTP as HTTP
import FFI.Math as Math
import FFI.Socket as Socket
import FFI.JSON as JSON

import Control.Tier3 as Tier3

import Control.DSL as DSL

import Control.Forward as Forward
import Control.Report (URI(..), uri) as Report

import Control.Route (Route)
import Control.Route as Route

import Data.IPv4 (IPv4(..))

import Data.Audit as Audit
import Data.Schema as Schema

import Text.Parsing.Common (ipv4)
import Text.Parsing.Report (event) as Report

data Role = Production | Testing

data URI = Primary Role | Secondary Role | Offsite Role

data Server = Single URI

data Authorization = Authorization Unit

data Authentication = Origin
  { sIP   :: IPv4
  , sPort :: Int
  }

data Settings = Settings Authorization Authentication Server

data AuthenticationType = Bearer

type Resource = Tier3.Resource

data Response = Ok Resource | InternalServerError String | BadRequest String | Forbidden AuthenticationType String


type Query a = DSL.Query Settings Resource Forward.URI Report.URI a

type Request a = DSL.Request Settings Resource Forward.URI Report.URI a

type Result a = DSL.Result a

audit :: Tier3.Settings -> Audit.Event -> Tier3.Request Unit
audit settings event = do
  _ <- Tier3.request settings $ Route.Forward (Forward.Audit event)
  pure unit

textJSON :: Resource -> String
textJSON (Tier3.Forward _) = ""
textJSON (Tier3.Report x)  = JSON.stringify $ unsafeCoerce x

sendResponse :: Response -> HTTP.ServerResponse -> Aff Unit
sendResponse (Ok body) = \res -> liftEffect $ do
  _ <- HTTP.setHeader "Content-Type" "text/json" $ res
  _ <- HTTP.writeHead 200 $ res
  _ <- HTTP.write (textJSON body) $ res
  _ <- HTTP.end $ res
  pure unit
sendResponse (BadRequest _) = \res -> liftEffect $ do
  _ <- HTTP.writeHead 400 $ res
  _ <- HTTP.end $ res
  pure unit  
sendResponse (Forbidden Bearer realm) = \res -> liftEffect $ do
  _ <- HTTP.setHeader "WWW-Authenticate" ("Bearer realm=" <> realm) $ res
  _ <- HTTP.writeHead 401 $ res
  _ <- HTTP.end $ res
  pure unit  
sendResponse (InternalServerError _) = \res -> liftEffect $ do
  _ <- HTTP.writeHead 500 $ res
  _ <- HTTP.end $ res
  pure unit

databaseRequest :: Tier3.Settings -> Route -> HTTP.IncomingMessage -> Aff (Response) 
databaseRequest settings route req = do
  result    <- Tier3.execute $ Tier3.request settings route
  case result of 
    (Left _)          -> pure  $ InternalServerError ""
    (Right resultSet) -> pure  $ Ok resultSet

resourceRequest :: Tier3.Settings -> HTTP.IncomingRequest -> Aff Unit
resourceRequest settings (HTTP.IncomingRequest req res) = do
  startTime     <- liftEffect $ Date.current
  routingResult <- Route.execute req
  resource      <- case routingResult of
                     (Left _)      -> pure $ BadRequest (HTTP.messageURL req)
                     (Right route) -> databaseRequest settings route req 
  response      <- try $ sendResponse resource res 
  endTime       <- liftEffect $ Date.current
  duration      <- pure $ Math.floor ((Date.getTime endTime) - (Date.getTime startTime))
  eventID       <- pure $ case routingResult of
                     (Left _)                                       -> Audit.Anomalous
                     (Right (Route.Forward (Forward.Audit _)))      -> Audit.Forward Schema.Audit
                     (Right (Route.Forward (Forward.Alert _)))      -> Audit.Forward Schema.Alert
                     (Right (Route.Forward (Forward.Flow _)))       -> Audit.Forward Schema.Flow
                     (Right (Route.Forward (Forward.Report _)))     -> Audit.Forward Schema.Report
                     (Right (Route.Forward (Forward.Linux  _)))     -> Audit.Forward Schema.Linux
                     (Right (Route.Forward (Forward.Windows _)))    -> Audit.Forward Schema.Windows
                     (Right (Route.Report  (Report.Audit _ _ _ _))) -> Audit.Report  Schema.Audit
  eventType     <- pure $ case routingResult of
                     (Left  _) -> Audit.Failure
                     (Right _) -> case resource of
                       (Ok _) -> Audit.Success
                       _      -> case response of
                         (Left _)  -> Audit.Failure
                         (Right _) -> Audit.Success
  sIP           <- pure $ case (runParser (Socket.remoteAddress $ HTTP.socket req) ipv4) of
                     (Left _)  -> (IPv4 (-1) (-1) (-1) (-1))
                     (Right x) -> x
  sPort         <- pure $ Socket.remotePort $ HTTP.socket req
  event         <- pure $ Audit.Event $
                     { eventCategory : Audit.Tier2
                     , eventType     : eventType
                     , eventID       : eventID
                     , startTime     : startTime
                     , duration      : duration
                     , endTime       : endTime
                     , sIP           : sIP
                     , sPort         : sPort
                     }
  _             <- Tier3.execute $ audit settings event
  pure unit

producer :: HTTP.Server -> Producer HTTP.IncomingRequest Aff Unit
producer server = produce \emitter -> do
  HTTP.onRequest (\req res -> emit emitter $ HTTP.IncomingRequest req res) $ server

consumer :: Tier3.Settings -> Consumer HTTP.IncomingRequest Aff Unit
consumer settings = forever $ do
  request' <- await
  _       <- lift $ resourceRequest settings request'
  pure unit

process :: Tier3.Settings -> HTTP.Server -> Process Aff Unit
process settings server = pullFrom (consumer settings) (producer server)

location :: URI -> String
location (Primary Production)   = "http://localhost:3000"
location (Secondary Production) = "http://localhost:3001"
location (Offsite Production)   = "http://localhost:3002"
location (Primary Testing)      = "http://localhost:3003"
location (Secondary Testing)    = "http://localhost:3004"
location (Offsite Testing)      = "http://localhost:3005"

executeForward :: Settings -> Forward.URI -> Aff Resource
executeForward (Settings _ _ (Single uri)) query = do
  req <- HTTP.createRequest HTTP.Post $ (location uri) <> show (Forward.uri query)
  res <- HTTP.endRequest req
  case res of
    (HTTP.IncomingResponse _ req') -> 
      case HTTP.statusCode req' of
        200 -> pure $ Tier3.Forward unit
        _   -> liftEffect $ Exception.throw "Invalid status code (forward reply)."

executeReport :: Settings -> Report.URI -> Aff Resource
executeReport (Settings _ _ (Single uri)) query = do
  req <- HTTP.createRequest HTTP.Get $ (location uri) <> show (Report.uri query)
  res <- HTTP.endRequest req
  case res of
    (HTTP.IncomingResponse body req') ->
       case HTTP.statusCode req' of
         200 ->
           case runParser body Report.event of
             (Left _)      -> liftEffect $ Exception.throw "Invalid body (report reply)."
             (Right event) -> pure $ Tier3.Report event
         _   -> liftEffect $ Exception.throw "Invalid status code (report reply)."

interpret :: forall a. Query (Request a) -> Aff (Request a)
interpret (DSL.Forward settings query next) = do
  result <- executeForward settings query
  next <$> pure result
interpret (DSL.Report settings query next) = do
  result <- executeReport settings query
  next <$> (pure result)

request :: Settings -> Route -> Request Resource
request settings (Route.Forward query) = liftFreeT $ (DSL.Forward settings query identity)
request settings (Route.Report query)  = liftFreeT $ (DSL.Report settings query identity)

execute ::  forall a. Request a -> Aff (Result a)
execute = try <<< runFreeT interpret
