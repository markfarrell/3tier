module Forward
  (Forward(..)
  , uri
  ) where

import Prelude

import Audit as Audit
import Data.Flow as Flow

data Forward = Flow Flow.Event | Audit Audit.Event

uri :: Forward -> String
uri (Flow event)  = "/forward/flow?"  <> Flow.uri event
uri (Audit event) = "/forward/audit?" <> Audit.uri event
