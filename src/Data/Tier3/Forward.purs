module Data.Tier3.Forward
  ( URI(..)
  , uri
  ) where

import Prelude

import Data.Audit as Audit
import Data.Alert as Alert
import Data.Flow as Flow
import Data.Linux as Linux
import Data.Report as Report
import Data.Windows as Windows

data URI = Audit Audit.Event | Alert Alert.Event | Flow Flow.Event | Report Report.Event | Linux Linux.Event | Windows Windows.Event 

uri :: URI -> String
uri (Audit event)   = "/forward/audit?"   <> show event
uri (Alert event)   = "/forward/audit?"   <> show event
uri (Flow event)    = "/forward/flow?"    <> show event
uri (Linux event)   = "/forward/linux?"   <> show event
uri (Report event)  = "/forward/report?"  <> show event
uri (Windows event) = "/forward/windows?" <> show event
