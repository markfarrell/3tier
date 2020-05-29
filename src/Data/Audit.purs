module Data.Audit
  ( Event(..)
  , EventCategory(..)
  , EventID(..)
  ) where

import Prelude

import Foreign.Class (class Marshall) as F
import Foreign.Coerce (coerce) as F

import Data.Event as E
import Data.EventType (EventType)

data EventID = Alert | Audit | Traffic | Windows | Linux

data EventCategory = Forward | Report

type Event = E.Event EventCategory EventType EventID

instance showEventIDAudit :: Show EventID where
  show Alert      = "ALERT"
  show Audit      = "AUDIT"
  show Traffic    = "TRAFFIC"
  show Linux      = "LINUX"
  show Windows    = "WINDOWS"

instance showEventCategory :: Show EventCategory where
  show Forward   = "FORWARD"
  show Report    = "REPORT"

derive instance eqEventCategoryAudit :: Eq EventCategory

derive instance eqEventIDAudit :: Eq EventID

instance marshallEventCategory :: F.Marshall EventCategory where
  marshall = F.coerce <<< show

instance marshallEventID :: F.Marshall EventID where
  marshall = F.coerce <<< show

instance eventCategoryAudit :: E.EventCategory EventCategory where
  eventCategories = [ Forward, Report ]

instance eventIDAudit :: E.EventID EventID where
  eventIDs = [ Alert, Audit, Traffic, Windows, Linux ]
