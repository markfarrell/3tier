module Tier2 
  ( Settings(..)
  , execute
  , process
  ) where 
  
import Prelude

import Control.Coroutine (Producer, Consumer, Process, pullFrom, await)
import Control.Coroutine.Aff (produce, emit)
import Control.Monad.Error.Class (try)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)

import Data.Either (Either(..))

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)

import Unsafe.Coerce (unsafeCoerce)

import FFI.Date as Date
import FFI.HTTP as HTTP
import FFI.Math as Math
import FFI.Socket as Socket
import FFI.JSON as JSON

import Data.Schema as Schema

import Tier3 as Tier3

import Audit as Audit

import Forward (Forward)
import Forward as Forward

import Report (Report)
import Report as Report

import Route (Route)
import Route as Route

data Settings = Settings { host :: String, port :: Int }

data ContentType a = TextJSON a

data AuthenticationType = Bearer

data Resource = Ok (ContentType Tier3.Resource) | InternalServerError String | BadRequest String | Forbidden AuthenticationType String

audit :: Tier3.Settings -> Audit.Event -> Tier3.Request Unit
audit settings event = do
  _ <- Tier3.request settings $ Route.Forward (Forward.Audit event)
  pure unit

textJSON :: Tier3.Resource -> String
textJSON (Tier3.Forward _) = ""
textJSON (Tier3.Report x)  = JSON.stringify $ unsafeCoerce x

sendResource :: Resource -> HTTP.ServerResponse -> Aff Unit
sendResource (Ok (TextJSON body)) = \res -> liftEffect $ do
  _ <- HTTP.setHeader "Content-Type" "text/json" $ res
  _ <- HTTP.writeHead 200 $ res
  _ <- HTTP.write (textJSON body) $ res
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

databaseRequest :: Tier3.Settings -> Route -> HTTP.IncomingMessage -> Aff (Resource) 
databaseRequest settings route req = do
  result    <- Tier3.execute $ Tier3.request settings route
  case result of 
    (Left _)          -> pure  $ InternalServerError ""
    (Right resultSet) -> pure  $ Ok (TextJSON resultSet)

resourceRequest :: Tier3.Settings -> HTTP.IncomingRequest -> Aff Unit
resourceRequest settings (HTTP.IncomingRequest req res) = do
  startTime     <- liftEffect $ Date.current
  routingResult <- Route.execute req
  resource      <- case routingResult of
                     (Left _)      -> pure $ BadRequest (HTTP.messageURL req)
                     (Right route) -> databaseRequest settings route req 
  response      <- try $ sendResource resource res 
  endTime       <- liftEffect $ Date.current
  duration      <- pure $ Math.floor ((Date.getTime endTime) - (Date.getTime startTime))
  eventID       <- pure $ case routingResult of
                     (Left _)                                     -> Audit.Invalid
                     (Right (Route.Forward (Forward.Flow _)))     -> Audit.Forward Schema.Flow
                     (Right (Route.Forward (Forward.Audit _)))    -> Audit.Forward Schema.Audit
                     (Right (Route.Report  (Report.Audit _ _ _))) -> Audit.Report  Schema.Audit
  eventType     <- pure $ case routingResult of
                     (Left  _) -> Audit.Failure
                     (Right _) -> case resource of
                       (Ok _) -> Audit.Success
                       _      -> case response of
                         (Left _)  -> Audit.Failure
                         (Right _) -> Audit.Success
  event     <- pure $ Audit.Event $
                 { eventCategory : Audit.Tier2
                 , eventType     : eventType
                 , eventID       : eventID
                 , startTime     : startTime
                 , duration      : duration
                 , endTime       : endTime
                 , sIP           : Socket.remoteAddress $ HTTP.socket req
                 , sPort         : Socket.remotePort    $ HTTP.socket req
                 }
  _     <- Tier3.execute $ audit settings event
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

url :: Settings -> String -> String
url (Settings settings) uri = location <> uri
  where location = "http://" <> settings.host <> ":" <> show settings.port

executeForward :: Settings -> Forward -> Aff (Either Error HTTP.IncomingResponse)
executeForward settings query = do
  req <- HTTP.createRequest HTTP.Post $ url settings (Forward.uri query)
  res <- try $ HTTP.endRequest req
  pure res

executeReport :: Settings -> Report -> Aff (Either Error HTTP.IncomingResponse)
executeReport settings query = do
  req <- HTTP.createRequest HTTP.Get $ url settings (Report.uri query)
  res <- try $ HTTP.endRequest req
  pure res

execute :: Settings -> Route -> Aff (Either Error HTTP.IncomingResponse)
execute settings (Route.Forward query) = executeForward settings query
execute settings (Route.Report query)  = executeReport settings query
