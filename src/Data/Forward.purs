module Data.Forward
  ( URI(..)
  ) where

import Prelude

import Data.Audit as Audit
import Data.Flow as Flow
import Data.Linux as Linux
import Data.Statistics as Statistics
import Data.Windows as Windows

data URI = Audit Audit.Event 
  | Flow Flow.Event 
  | Statistics Statistics.Event 
  | Windows Windows.Event 

instance showURIForward :: Show URI where
  show = uri

instance eqURIForward :: Eq URI where
  eq (Flow x) (Flow y)        = (x == y)
  eq (Windows x) (Windows y)  = (x == y)
  eq _        _               = false

uri :: URI -> String
uri (Audit event)      = "/forward/audit?"   <> show event
uri (Flow event)       = "/forward/flow?"    <> show event
uri (Statistics event) = "/forward/statistics?"  <> show event
uri (Windows event)    = "/forward/windows?" <> show event
