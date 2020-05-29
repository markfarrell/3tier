module Data.EventType
  ( EventType (..)
  ) where

import Prelude

import Foreign.Class (class Marshall) as F
import Foreign.Coerce (coerce) as F

import Data.Event as E

{-- todo: see https://github.com/markfarrell/3tier/issues/5 --}

data EventType = Success | Failure

instance showEventType :: Show EventType where
  show Success = "SUCCESS"
  show Failure = "FAILURE"

derive instance eqEventType :: Eq EventType

instance marshallEventType :: F.Marshall EventType where
  marshall = F.coerce <<< show

instance eventTypeDefault :: E.EventType EventType where
  eventTypes = [ Success,  Failure ]
