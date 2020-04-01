module Forward
  (Forward(..)
  , uri
  ) where

import Prelude

import Audit as Audit
import Flow as Flow

data Forward = Flow Flow.Record | Audit Audit.Record

uri :: Forward -> String
uri (Flow record)  = "/forward/flow"  <> Flow.uri record
uri (Audit record) = "/forward/audit" <> Audit.uri record
