module Control.Tier2 
  ( Settings(..)
  , Authentication(..)
  , Authorization(..)
  , Target(..)
  , Role(..)
  , URI(..)
  , Query
  , Request
  , Resource(..)
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
import FFI.HTTPS as HTTPS
import FFI.Math as Math
import FFI.Socket as Socket
import FFI.JSON as JSON

import Control.Tier3 as Tier3

import Control.DSL as DSL

import Control.Authorization as Authorization
import Control.Authentication as Authentication

import Control.Forward as Forward
import Control.Report (URI(..)) as Report

import Control.Route (Route)
import Control.Route as Route

import Data.IPv4 (IPv4(..))

import Data.Audit as Audit
import Data.Report (Event) as Report
import Data.Schema as Schema

import Text.Parsing.Common (ipv4)
import Text.Parsing.Report (event) as Report

data Role = Production | Testing

data URI = Primary Role | Secondary Role | Offsite Role

data Target = Single URI

data Authorization = Authorization Unit

data Authentication = Origin
  { sIP   :: IPv4
  , sPort :: Int
  }

data Settings = Settings Authorization Authentication Target

data AuthenticationType = Bearer

data Resource = Forward Unit | Report Report.Event

data Response = Ok Resource | InternalServerError String | BadRequest String | Forbidden AuthenticationType String

type Query a = DSL.Query Settings Resource Forward.URI Report.URI a

type Request a = DSL.Request Settings Resource Forward.URI Report.URI a

type Result a = DSL.Result a

audit :: Tier3.Settings -> Audit.Event -> Tier3.Request Unit
audit settings event = do
  _ <- Tier3.request settings $ Route.Forward (Forward.Audit event)
  pure unit

textJSON :: Resource -> String
textJSON (Forward _) = ""
textJSON (Report x)  = JSON.stringify $ unsafeCoerce x

sendResponse :: Response -> HTTPS.ServerResponse -> Aff Unit
sendResponse (Ok body) = \res -> liftEffect $ do
  _ <- HTTPS.setHeader "Content-Type" "text/json" $ res
  _ <- HTTPS.writeHead 200 $ res
  _ <- HTTPS.write (textJSON body) $ res
  _ <- HTTPS.end $ res
  pure unit
sendResponse (BadRequest _) = \res -> liftEffect $ do
  _ <- HTTPS.writeHead 400 $ res
  _ <- HTTPS.end $ res
  pure unit  
sendResponse (Forbidden Bearer realm) = \res -> liftEffect $ do
  _ <- HTTPS.setHeader "WWW-Authenticate" ("Bearer realm=" <> realm) $ res
  _ <- HTTPS.writeHead 401 $ res
  _ <- HTTPS.end $ res
  pure unit  
sendResponse (InternalServerError _) = \res -> liftEffect $ do
  _ <- HTTPS.writeHead 500 $ res
  _ <- HTTPS.end $ res
  pure unit

databaseRequest :: Tier3.Settings -> Route -> HTTPS.IncomingMessage -> Aff Response 
databaseRequest settings route req = do
  result    <- Tier3.execute $ Tier3.request settings route
  case result of 
    (Left _)               -> pure  $ InternalServerError ""
    (Right (Tier3.Forward unit)) -> pure $ Ok (Forward unit)
    (Right (Tier3.Report event)) -> pure $ Ok (Report event) 

resourceRequest :: Tier3.Settings -> HTTPS.IncomingRequest -> Aff Unit
resourceRequest settings (HTTPS.IncomingRequest req res) = do
  startTime     <- liftEffect $ Date.current
  routingResult <- Route.execute req
  resource      <- case routingResult of
                     (Left _)      -> pure $ BadRequest (HTTPS.messageURL req)
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
  sIP           <- pure $ case (runParser (Socket.remoteAddress $ HTTPS.socket req) ipv4) of
                     (Left _)  -> (IPv4 (-1) (-1) (-1) (-1))
                     (Right x) -> x
  sPort         <- pure $ Socket.remotePort $ HTTPS.socket req
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

producer :: HTTPS.Server -> Producer HTTPS.IncomingRequest Aff Unit
producer server = produce \emitter -> do
  HTTPS.onRequest (\req res -> emit emitter $ HTTPS.IncomingRequest req res) $ server

consumer :: Tier3.Settings -> Consumer HTTPS.IncomingRequest Aff Unit
consumer settings = forever $ do
  request' <- await
  _       <- lift $ resourceRequest settings request'
  pure unit

process :: HTTPS.Server -> Process Aff Unit
process server = pullFrom (consumer settings) (producer server)
  where
    settings          = Tier3.Settings Authorization.Default Authentication.Default (Tier3.Failover (Tier3.Primary Tier3.Production))

path :: URI -> String
path (Primary Production)   = "http://localhost:3000"
path (Secondary Production) = "http://localhost:3001"
path (Offsite Production)   = "http://localhost:3002"
path (Primary Testing)      = "http://localhost:3003"
path (Secondary Testing)    = "http://localhost:3004"
path (Offsite Testing)      = "http://localhost:3005"

executeForward :: Settings -> Forward.URI -> Aff Resource
executeForward (Settings _ _ (Single uri)) query = do
  req <- HTTPS.createRequest HTTPS.Post $ (path uri) <> show query
  res <- HTTPS.endRequest req
  case res of
    (HTTPS.IncomingResponse _ req') -> 
      case HTTPS.statusCode req' of
        200 -> pure $ Forward unit
        _   -> liftEffect $ Exception.throw "Invalid status code (forward reply)."

executeReport :: Settings -> Report.URI -> Aff Resource
executeReport (Settings _ _ (Single uri)) query = do
  req <- HTTPS.createRequest HTTPS.Get $ (path uri) <> show query
  res <- HTTPS.endRequest req
  case res of
    (HTTPS.IncomingResponse body req') ->
       case HTTPS.statusCode req' of
         200 ->
           case runParser body Report.event of
             (Left _)      -> liftEffect $ Exception.throw "Invalid body (report reply)."
             (Right event) -> pure $ Report event
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
