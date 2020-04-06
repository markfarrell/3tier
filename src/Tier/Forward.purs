module Tier.Forward
  (Forward(..)
  , uri
  ) where

import Prelude

import Data.Audit as Audit
import Data.Flow as Flow
import Data.Linux as Linux
import Data.Windows as Windows

data Forward = Flow Flow.Event | Audit Audit.Event | Linux Linux.Event | Windows Windows.Event 

uri :: Forward -> String
uri (Audit event)   = "/forward/audit?"   <> show event
uri (Flow event)    = "/forward/flow?"    <> show event
uri (Linux event)   = "/forward/linux?"   <> show event
uri (Windows event) = "/forward/windows?" <> show event
