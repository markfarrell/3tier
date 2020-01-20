module SIEM.Logging.Session
  ( getLogID
  , createLogID
  , echoLogID
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)

import Data.Either (Either(..))

import Foreign (readString) as Foreign
import Foreign.Index ((!))

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (error) as Exception

import HTTP as HTTP
import UUIDv1 as UUIDv1

getLogID :: HTTP.IncomingMessage -> Aff String
getLogID req = do
   result <- pure (getLogID' $ HTTP.messageHeaders req)
   case result of
     (Left _)      -> throwError $ Exception.error "Invalid request headers (Log-ID)."
     (Right logID) -> pure $ logID
  where
    getLogID' headers = runExcept $ do
      header <- headers ! "log-id" >>= Foreign.readString
      pure $ header

createLogID :: Aff String
createLogID = do
  uuid <- liftEffect $ UUIDv1.createUUID
  pure uuid

echoLogID :: HTTP.IncomingRequest -> Aff Unit
echoLogID (HTTP.IncomingRequest req res) = do
  result <- pure $ runExcept $ headers ! header >>= Foreign.readString
  case result of
    (Left _)      -> pure unit
    (Right logID) -> liftEffect $ HTTP.setHeader header logID $ res 
  where
    header = "log-id"
    headers = HTTP.messageHeaders req

