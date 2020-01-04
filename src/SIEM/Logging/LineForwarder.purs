module SIEM.Logging.LineForwarder
  where

import Prelude

import Control.Coroutine (Producer, Consumer, Process, pullFrom, await, runProcess)
import Control.Coroutine.Aff (produce, emit)
import Control.Monad.Error.Class (try)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)

import Data.Array (drop) as Array
import Data.Either (Either (..))

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log) as Console
import Effect.Aff (Aff, launchAff)

import Readline as Readline

import HTTP as HTTP
import Process as Process
import Strings as Strings

import Audit as Audit

lines :: Readline.Interface -> Producer String Aff Unit
lines interface = produce \emitter -> do
  Readline.onLine (\line -> emit emitter $ line) $ interface

log :: String -> Aff Unit
log = liftEffect <<< Console.log

forwarder :: String -> Consumer String Aff Unit
forwarder url' = forever $ do
  line      <- await
  result    <- lift $ try $ post (url line)
  lift $ log' line result
  where 
    url  line  =  url' <> (Strings.encodeURIComponent line)
    post       =  HTTP.request HTTP.Post
    log' line = \result -> do
       case result of
         (Left error)                           -> Audit.debug $ Audit.Entry Audit.Failure Audit.ClientRequest (show [url line, show error])
         (Right (HTTP.IncomingResponse body _)) -> Audit.debug $ Audit.Entry Audit.Success Audit.ClientRequest (show [url line, body])

lineForwarder :: Readline.Interface -> String -> Process Aff Unit
lineForwarder interface = \url' -> pullFrom (forwarder url') (lines interface)

runLineForwarder :: Readline.Interface -> String -> Effect Unit
runLineForwarder interface = \url' -> void $ launchAff $ do 
  runProcess $ lineForwarder interface $ url'

main :: Effect Unit
main = do
  interface <- Readline.createInterface Process.stdin Process.stdout false
  case argv' of
    [url'] -> runLineForwarder interface $ url'
    _                 -> pure unit
  where argv'  = Array.drop 2 Process.argv

