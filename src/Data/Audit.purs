module Data.Audit
  ( Event(..)
  , EventCategory(..)
  , EventID(..)
  , eventCategories
  , eventIDs
  ) where

import Prelude

import Data.Event as Event

data EventType = Success | Failure

data EventID = Alert | Audit | Traffic | Windows | Linux | Statistics

data EventCategory = Forward | Report

type Event = Event.Event EventCategory EventID

instance showEventIDAudit :: Show EventID where
  show (Alert)        = "ALERT"
  show (Audit)        = "AUDIT"
  show (Traffic)      = "TRAFFIC"
  show (Linux)        = "LINUX"
  show (Statistics)   = "STATISTICS"
  show (Windows)      = "WINDOWS"

instance showEventCategory :: Show EventCategory where
  show Forward   = "FORWARD"
  show Report    = "REPORT"

derive instance eqEventCategoryAudit :: Eq EventCategory

derive instance eqEventIDAudit :: Eq EventID

eventCategories :: Array EventCategory
eventCategories = [Forward, Report]

eventIDs :: Array EventID
eventIDs =
  [ Audit
  , Alert
  , Traffic
  , Windows
  , Linux
  , Statistics
  ]
