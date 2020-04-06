module Forward
  (Forward(..)
  , uri
  ) where

import Prelude

import Data.Audit as Audit
import Data.Flow as Flow
import Data.Windows as Windows

data Forward = Flow Flow.Event | Audit Audit.Event | Windows Windows.Event

uri :: Forward -> String
uri (Flow event)  = "/forward/flow?"      <> show event
uri (Audit event) = "/forward/audit?"     <> show event
uri (Windows event) = "/forward/windows?" <> show event
