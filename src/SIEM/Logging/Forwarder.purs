module SIEM.Logging.Forwarder
  ( forwardWindows
  , forwardSensor
  , forwardLinux
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

import Effect.Exception (Error)

import HTTP as HTTP
import Process as Process
import Strings as Strings

import Audit as Audit

import SIEM.Logging.Windows as Windows
import SIEM.Logging.Sensor as Sensor
import SIEM.Logging.Linux as Linux

import UUIDv1 as UUIDv1

data ForwardType = Windows | Sensor | Linux

forwardType' :: ForwardType -> String
forwardType' Windows = "windows"
forwardType' Sensor = "sensor"
forwardType' Linux = "linux"

forward :: ForwardType -> String -> String ->  Aff HTTP.IncomingResponse
forward forwardType host entry = do
  req <- HTTP.createRequest HTTP.Post requestURL
  _   <- HTTP.setRequestHeader "Log-ID" logID $ req
  res <- HTTP.endRequest req
  pure res
  where
    requestURL = "http://" <> host <> "/forward/" <> (forwardType' forwardType) <> "?entry=" <> entry'
    entry' = Strings.encodeURIComponent entry
    logID = UUIDv1.defaultUUID

forwardWindows :: String -> String -> Aff HTTP.IncomingResponse
forwardWindows = forward Windows

forwardSensor :: String -> String -> Aff HTTP.IncomingResponse
forwardSensor = forward Sensor

forwardLinux :: String -> String -> Aff HTTP.IncomingResponse
forwardLinux = forward Linux

forwarder :: forall a. Show a => ForwardType -> (a -> Either Error String) -> String -> Consumer a Aff Unit
forwarder forwardType write host = forever $ do
  entry       <- await
  result      <- lift $ pure $ write entry
  case result of
    (Left error) -> do
       let result' = { entry : entry, error : error }
       lift $ Audit.debug $ Audit.Entry Audit.Failure Audit.SerializationRequest (show result')
    (Right entry') -> do
       let result' = { entry : entry, entry' : entry' }
       _ <- lift $ Audit.debug $ Audit.Entry Audit.Success Audit.SerializationRequest (show result')
       result'' <- lift $ try $ forward forwardType host entry'
       case result'' of
         (Left error)                           -> do
            let result''' = { entry : entry, error : error }
            lift $ Audit.debug $ Audit.Entry Audit.Failure Audit.ForwardRequest (show result''')
         (Right (HTTP.IncomingResponse body _)) -> do
            let result''' = { entry : entry, body : body }
            lift $ Audit.debug $ Audit.Entry Audit.Success Audit.ForwardRequest (show result''')

main :: Effect Unit
main = do
  case argv' of
    [host, "windows"] -> do
      producer  <- Windows.createReader Process.stdin
      consumer  <- pure $ forwarderWindows host
      void $ launchAff $ runProcess $ pullFrom consumer producer
    [host, "sensor"]  -> do
      producer  <- Sensor.createReader Process.stdin
      consumer  <- pure $ forwarderSensor host
      void $ launchAff $ runProcess $ pullFrom consumer producer
    [host, "linux"]   -> do
      producer  <- Linux.createReader Process.stdin
      consumer  <- pure $ forwarderLinux host
      void $ launchAff $ runProcess $ pullFrom consumer producer
    _                   -> pure unit
  where
    forwarderWindows = forwarder Windows Windows.writeEntry
    forwarderSensor  = forwarder Sensor Sensor.writeEntry
    forwarderLinux   = forwarder Linux Linux.writeEntry
    argv'  = Array.drop 2 Process.argv
