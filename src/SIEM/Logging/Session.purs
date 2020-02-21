module SIEM.Logging.Session
  ( getLogID
  , createLogID
  , echoLogID
  ) where

import Prelude

import Control.Monad.Error.Class (try, throwError)
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
  id <- liftEffect  $ UUIDv1.createUUID
  pure id

echoLogID :: HTTP.IncomingRequest -> Aff Unit
echoLogID (HTTP.IncomingRequest req res) = do
  result <- try $ getLogID req
  case result of
    (Left _)      -> pure unit
    (Right logID) -> liftEffect $ HTTP.setHeader "log-id" logID $ res 
