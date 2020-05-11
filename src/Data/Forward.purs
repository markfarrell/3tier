module Data.Forward
  ( Event(..)
  ) where

import Prelude

import Data.Audit as Audit

data Event = Audit Audit.Event 

instance showEventForward :: Show Event where
  show = uri

derive instance eqForwardEvent :: Eq Event

uri :: Event -> String
uri (Audit event)      = "/forward/audit?"   <> show event
