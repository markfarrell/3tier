module Forward
  (Forward(..)
  , uri
  ) where

import Prelude

import Data.Audit as Audit
import Data.Flow as Flow

data Forward = Flow Flow.Event | Audit Audit.Event

uri :: Forward -> String
uri (Flow event)  = "/forward/flow?"  <> show event
uri (Audit event) = "/forward/audit?" <> show event
