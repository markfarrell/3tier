module Data.Forward
  ( URI(..)
  ) where

import Prelude

import Data.Audit as Audit

data URI = Audit Audit.Event 

instance showURIForward :: Show URI where
  show = uri

derive instance eqForwardURI :: Eq URI

uri :: URI -> String
uri (Audit event)      = "/forward/audit?"   <> show event
