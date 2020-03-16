module Audit
  ( EventType(..)
  , EventID(..)
  , Entry(..)
  ) where

import Prelude

import Control.Monad.Trans.Class (lift)

import Data.Tuple (Tuple(..))

import Effect.Aff (Aff)
import Effect.Class (liftEffect)

data EventType = Success | Failure

data EventID = DatabaseRequest | ResourceRequest | ResourceResponse | RoutingRequest

data Entry = Entry EventType EventID String

instance showEventType :: Show EventType where
  show Success = "SUCCESS"
  show Failure = "FAILURE"

instance showEventID :: Show EventID where
  show DatabaseRequest  = "DATABASE-REQUEST"
  show ResourceRequest  = "RESOURCE-REQUEST"
  show ResourceResponse = "RESOURCE-RESPONSE"
  show RoutingRequest   = "ROUTING-REQUEST"

instance showEntry :: Show Entry where
  show (Entry eventType eventID msg) = "(Entry " <> show eventType <> " " <> show eventID <> " " <> show msg <> ")"
