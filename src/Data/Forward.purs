module Data.Forward
  ( Event(..)
  ) where

import Prelude

import Data.Alert   as Alert
import Data.Audit   as Audit
import Data.Traffic as Traffic
import Data.Linux   as Linux
import Data.Windows as Windows

data Event = 
    Alert Alert.Event
  | Audit Audit.Event 
  | Traffic Traffic.Event
  | Linux Linux.Event
  | Windows Windows.Event

instance showEventForward :: Show Event where
  show (Alert event)   = "/forward/alert?event="   <> show event
  show (Audit event)   = "/forward/audit?event="   <> show event
  show (Traffic event) = "/forward/traffic?event=" <> show event
  show (Linux event)   = "/forward/linux?event="   <> show event
  show (Windows event) = "/forward/windows?event=" <> show event

derive instance eqForwardEvent :: Eq Event
