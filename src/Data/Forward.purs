module Data.Forward
  ( Event(..)
  ) where

import Prelude

import Data.Audit as Audit

data Event = Audit Audit.Event 

instance showEventForward :: Show Event where
  show (Audit event) = "/forward/audit?" <> show event

derive instance eqForwardEvent :: Eq Event
