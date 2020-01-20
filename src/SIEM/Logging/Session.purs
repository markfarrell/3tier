module SIEM.Logging.Session
  ( getLogID
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)

import Data.Either (Either(..))

import Foreign (readString) as Foreign
import Foreign.Index ((!))

import Effect.Aff (Aff)
import Effect.Exception (error) as Exception

import HTTP as HTTP

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
