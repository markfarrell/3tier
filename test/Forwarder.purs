module Test.Forwarder
  ( forwardFlow
  , main
  ) where

import Prelude

import Control.Coroutine (Consumer, pullFrom, await, runProcess)
import Control.Monad.Error.Class (try)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)

import Data.Array (drop) as Array
import Data.Either (Either (..))

import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)

import Effect.Exception (Error)

import HTTP as HTTP
import Process as Process
import Strings as Strings

import Flow as Flow

data ForwardType = Flow

forwardType' :: ForwardType -> String
forwardType' Flow = "flow"

forward :: ForwardType -> String -> String -> Aff HTTP.IncomingResponse
forward forwardType host entry = do
  req <- HTTP.createRequest HTTP.Post requestURL 
  res <- HTTP.endRequest req
  pure res
  where
    requestURL = "http://" <> host <> "/forward/" <> (forwardType' forwardType) <> "?q=" <> entry'
    entry' = Strings.encodeURIComponent entry

forwardFlow :: String -> String -> Aff HTTP.IncomingResponse
forwardFlow = forward Flow

forwarder :: forall a. Show a => ForwardType -> (a -> Either Error String) -> String -> Consumer a Aff Unit
forwarder forwardType write host = forever $ do
  entry       <- await
  result      <- lift $ pure $ write entry
  case result of
    (Left error)   -> lift $ pure unit
    (Right entry') -> do 
       _ <- lift $ try $ forward forwardType host entry'
       pure unit

main :: Effect Unit
main = do
  case argv' of
    [host, "flow"]  -> void $ launchAff $ do
      producer  <- liftEffect (Flow.createReader Process.stdin)
      consumer  <- pure $ forwarderFlow host
      runProcess $ pullFrom consumer producer
    _ -> pure unit
  where
    forwarderFlow  = forwarder Flow Flow.writeEntry
    argv'  = Array.drop 2 Process.argv
