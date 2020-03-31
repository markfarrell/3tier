module Forward
  (Forward(..))
  where

import Audit as Audit
import Flow as Flow

data Forward = Flow Flow.Entry | Audit Audit.Entry
