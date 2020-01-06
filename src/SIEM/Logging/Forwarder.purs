module SIEM.Logging.Forwarder
  where

import Prelude

import Control.Coroutine (Consumer, pullFrom, await, runProcess)
import Control.Monad.Error.Class (try)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)

import Data.Array (drop) as Array
import Data.Either (Either (..))

import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Exception (Error)

import HTTP as HTTP
import Process as Process
import Strings as Strings

import Audit as Audit

import Windows as Windows
import Sensor as Sensor

forwarder :: forall a. Show a => (a -> Either Error String) -> String -> Consumer a Aff Unit
forwarder write url' = forever $ do
  entry       <- await
  result      <- lift $ pure $ write entry
  case result of
    (Left error) -> do
       let result' = { url' : url', entry : entry, error : error }
       lift $ Audit.debug $ Audit.Entry Audit.Failure Audit.SerializationRequest (show result')
    (Right entry') -> do
       let result' = { url' : url', entry : entry, entry' : entry' }
       _ <- lift $ Audit.debug $ Audit.Entry Audit.Success Audit.SerializationRequest (show result')
       result'' <- lift $ try $ post (url entry')
       case result'' of
         (Left error)                           -> do
            let result''' = { url : url entry', entry : entry, error : error }
            lift $ Audit.debug $ Audit.Entry Audit.Failure Audit.ClientRequest (show result''')
         (Right (HTTP.IncomingResponse body _)) -> do
            let result''' = { url : url entry', entry : entry, body : body }
            lift $ Audit.debug $ Audit.Entry Audit.Success Audit.ClientRequest (show result''')
  where 
    url entry'  =  url' <> (Strings.encodeURIComponent entry')
    post        =  HTTP.request HTTP.Post

main :: Effect Unit
main = do
  case argv' of
    ["--windows", host] -> do
      producer <- Windows.createReader Process.stdin
      consumer <- pure $ forwarder Windows.writeEntry $ "http://" <> host <> "/forward/windows?entry="
      void $ launchAff $ runProcess $ pullFrom consumer producer
    ["--sensor", host] -> do
      producer <- Sensor.createReader Process.stdin
      consumer <- pure $ forwarder Sensor.writeEntry $ "http://" <> host <> "/forward/sensor?entry="
      void $ launchAff $ runProcess $ pullFrom consumer producer
    _                   -> pure unit
  where argv'  = Array.drop 2 Process.argv
