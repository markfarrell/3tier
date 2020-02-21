module SIEM.Logging.Session
  ( getLogID
  ) where

import Prelude

import Control.Monad.Except (runExcept)

import Data.Either (Either(..))

import Foreign (readString) as Foreign
import Foreign.Index ((!))

import Effect.Aff (Aff)

import HTTP as HTTP
import UUIDv1 as UUIDv1

getLogID :: HTTP.IncomingMessage -> Aff String
getLogID req = do
   result <- pure (getLogID' $ HTTP.messageHeaders req)
   case result of
     (Left _)      -> pure $ UUIDv1.defaultUUID
     (Right logID) -> pure $ logID
  where
    getLogID' headers = runExcept $ do
      header <- headers ! "log-id" >>= Foreign.readString
      pure $ header
