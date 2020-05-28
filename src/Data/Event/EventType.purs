module Data.Event.EventType
  ( EventType (..)
  ) where

import Prelude

{-- todo: see https://github.com/markfarrell/3tier/issues/5 --}

data EventType = Success | Failure

instance showEventType :: Show EventType where
  show Success = "SUCCESS"
  show Failure = "FAILURE"

derive instance eqEventType :: Eq EventType
