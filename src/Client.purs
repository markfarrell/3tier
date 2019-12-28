module Client
  ( launchProcess
  ) where

import Prelude

import Control.Coroutine (Producer, Consumer, Process, pullFrom, await, runProcess)
import Control.Coroutine.Aff (produce, emit)
import Control.Monad.Error.Class (try)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)

import Data.Either (Either (..))

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log) as Console
import Effect.Aff (Aff, launchAff)

import HTTP as HTTP
import Readline as Readline
import Strings as Strings

import Audit as Audit

producer :: Readline.Interface -> Producer String Aff Unit
producer interface = produce \emitter -> do
  Readline.onLine (\line -> emit emitter $ line) $ interface

log :: String -> Aff Unit
log = liftEffect <<< Console.log

consumer :: String -> Consumer String Aff Unit
consumer url' = forever $ do
  line      <- await
  result    <- lift $ try $ post (url line)
  lift $ log' line result
  where 
    url  line  =  url' <> (Strings.encodeURIComponent line)
    post       =  HTTP.request HTTP.Post
    log' line = \result -> do
       case result of
         (Left error)                           -> Audit.log $ Audit.Entry Audit.Failure Audit.ClientRequest (show [url line, show error])
         (Right (HTTP.IncomingResponse body _)) -> Audit.log $ Audit.Entry Audit.Success Audit.ClientRequest (show [url line, body])

process :: String -> Readline.Interface -> Process Aff Unit
process url' interface = pullFrom (consumer url') (producer interface)

launchProcess :: String -> Readline.Interface -> Effect Unit
launchProcess url' interface = void $ launchAff $ do 
  runProcess $ process url' interface
