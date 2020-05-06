module Control.Report
  ( URI(..)
  , uris
  ) where

import Prelude

import Data.Audit as Audit
import Data.Schema as Schema

data URI = Audit Audit.EventCategory Audit.EventType Audit.EventID Audit.ReportType

instance showURIReport :: Show URI where
  show = uri

uris :: Array URI
uris = anomalies <> forwards <> reports

anomalies :: Array URI
anomalies =
  [ Audit Audit.Tier3 Audit.Failure Audit.Anomalous Audit.Source  
  , Audit Audit.Tier3 Audit.Success Audit.Anomalous Audit.Duration
  , Audit Audit.Tier3 Audit.Failure Audit.Anomalous Audit.Duration
  , Audit Audit.Tier2 Audit.Success Audit.Anomalous Audit.Source  
  , Audit Audit.Tier2 Audit.Failure Audit.Anomalous Audit.Source  
  , Audit Audit.Tier2 Audit.Success Audit.Anomalous Audit.Duration
  , Audit Audit.Tier2 Audit.Failure Audit.Anomalous Audit.Duration
  , Audit Audit.Tier1 Audit.Success Audit.Anomalous Audit.Source  
  , Audit Audit.Tier1 Audit.Failure Audit.Anomalous Audit.Source  
  , Audit Audit.Tier1 Audit.Success Audit.Anomalous Audit.Duration
  , Audit Audit.Tier1 Audit.Failure Audit.Anomalous Audit.Duration
  ]

forwards :: Array URI
forwards =
  [ Audit Audit.Tier3 Audit.Success (Audit.Forward Schema.Audit) Audit.Source  
  , Audit Audit.Tier3 Audit.Failure (Audit.Forward Schema.Audit) Audit.Source  
  , Audit Audit.Tier3 Audit.Success (Audit.Forward Schema.Audit) Audit.Duration
  , Audit Audit.Tier3 Audit.Failure (Audit.Forward Schema.Audit) Audit.Duration
  , Audit Audit.Tier2 Audit.Success (Audit.Forward Schema.Audit) Audit.Source  
  , Audit Audit.Tier2 Audit.Failure (Audit.Forward Schema.Audit) Audit.Source  
  , Audit Audit.Tier2 Audit.Success (Audit.Forward Schema.Audit) Audit.Duration
  , Audit Audit.Tier2 Audit.Failure (Audit.Forward Schema.Audit) Audit.Duration
  , Audit Audit.Tier1 Audit.Success (Audit.Forward Schema.Audit) Audit.Source  
  , Audit Audit.Tier1 Audit.Failure (Audit.Forward Schema.Audit) Audit.Source  
  , Audit Audit.Tier1 Audit.Success (Audit.Forward Schema.Audit) Audit.Duration
  , Audit Audit.Tier1 Audit.Failure (Audit.Forward Schema.Audit) Audit.Duration
  , Audit Audit.Tier3 Audit.Success (Audit.Forward Schema.Alert) Audit.Source  

  , Audit Audit.Tier3 Audit.Failure (Audit.Forward Schema.Alert) Audit.Source  
  , Audit Audit.Tier3 Audit.Success (Audit.Forward Schema.Alert) Audit.Duration
  , Audit Audit.Tier3 Audit.Failure (Audit.Forward Schema.Alert) Audit.Duration
  , Audit Audit.Tier2 Audit.Success (Audit.Forward Schema.Alert) Audit.Source  
  , Audit Audit.Tier2 Audit.Failure (Audit.Forward Schema.Alert) Audit.Source  
  , Audit Audit.Tier2 Audit.Success (Audit.Forward Schema.Alert) Audit.Duration
  , Audit Audit.Tier2 Audit.Failure (Audit.Forward Schema.Alert) Audit.Duration
  , Audit Audit.Tier1 Audit.Success (Audit.Forward Schema.Alert) Audit.Source  
  , Audit Audit.Tier1 Audit.Failure (Audit.Forward Schema.Alert) Audit.Source  
  , Audit Audit.Tier1 Audit.Success (Audit.Forward Schema.Alert) Audit.Duration
  , Audit Audit.Tier1 Audit.Failure (Audit.Forward Schema.Alert) Audit.Duration

  , Audit Audit.Tier3 Audit.Failure (Audit.Forward Schema.Flow) Audit.Source  
  , Audit Audit.Tier3 Audit.Success (Audit.Forward Schema.Flow) Audit.Duration
  , Audit Audit.Tier3 Audit.Failure (Audit.Forward Schema.Flow) Audit.Duration
  , Audit Audit.Tier2 Audit.Success (Audit.Forward Schema.Flow) Audit.Source  
  , Audit Audit.Tier2 Audit.Failure (Audit.Forward Schema.Flow) Audit.Source  
  , Audit Audit.Tier2 Audit.Success (Audit.Forward Schema.Flow) Audit.Duration
  , Audit Audit.Tier2 Audit.Failure (Audit.Forward Schema.Flow) Audit.Duration
  , Audit Audit.Tier1 Audit.Success (Audit.Forward Schema.Flow) Audit.Source  
  , Audit Audit.Tier1 Audit.Failure (Audit.Forward Schema.Flow) Audit.Source  
  , Audit Audit.Tier1 Audit.Success (Audit.Forward Schema.Flow) Audit.Duration
  , Audit Audit.Tier1 Audit.Failure (Audit.Forward Schema.Flow) Audit.Duration

  , Audit Audit.Tier3 Audit.Failure (Audit.Forward Schema.Statistics) Audit.Source  
  , Audit Audit.Tier3 Audit.Success (Audit.Forward Schema.Statistics) Audit.Duration
  , Audit Audit.Tier3 Audit.Failure (Audit.Forward Schema.Statistics) Audit.Duration
  , Audit Audit.Tier2 Audit.Success (Audit.Forward Schema.Statistics) Audit.Source  
  , Audit Audit.Tier2 Audit.Failure (Audit.Forward Schema.Statistics) Audit.Source  
  , Audit Audit.Tier2 Audit.Success (Audit.Forward Schema.Statistics) Audit.Duration
  , Audit Audit.Tier2 Audit.Failure (Audit.Forward Schema.Statistics) Audit.Duration
  , Audit Audit.Tier1 Audit.Success (Audit.Forward Schema.Statistics) Audit.Source  
  , Audit Audit.Tier1 Audit.Failure (Audit.Forward Schema.Statistics) Audit.Source  
  , Audit Audit.Tier1 Audit.Success (Audit.Forward Schema.Statistics) Audit.Duration
  , Audit Audit.Tier1 Audit.Failure (Audit.Forward Schema.Statistics) Audit.Duration

  , Audit Audit.Tier3 Audit.Failure (Audit.Forward Schema.Linux) Audit.Source  
  , Audit Audit.Tier3 Audit.Success (Audit.Forward Schema.Linux) Audit.Duration
  , Audit Audit.Tier3 Audit.Failure (Audit.Forward Schema.Linux) Audit.Duration
  , Audit Audit.Tier2 Audit.Success (Audit.Forward Schema.Linux) Audit.Source  
  , Audit Audit.Tier2 Audit.Failure (Audit.Forward Schema.Linux) Audit.Source  
  , Audit Audit.Tier2 Audit.Success (Audit.Forward Schema.Linux) Audit.Duration
  , Audit Audit.Tier2 Audit.Failure (Audit.Forward Schema.Linux) Audit.Duration
  , Audit Audit.Tier1 Audit.Success (Audit.Forward Schema.Linux) Audit.Source  
  , Audit Audit.Tier1 Audit.Failure (Audit.Forward Schema.Linux) Audit.Source  
  , Audit Audit.Tier1 Audit.Success (Audit.Forward Schema.Linux) Audit.Duration
  , Audit Audit.Tier1 Audit.Failure (Audit.Forward Schema.Linux) Audit.Duration

  , Audit Audit.Tier3 Audit.Failure (Audit.Forward Schema.Windows) Audit.Source  
  , Audit Audit.Tier3 Audit.Success (Audit.Forward Schema.Windows) Audit.Duration
  , Audit Audit.Tier3 Audit.Failure (Audit.Forward Schema.Windows) Audit.Duration
  , Audit Audit.Tier2 Audit.Success (Audit.Forward Schema.Windows) Audit.Source  
  , Audit Audit.Tier2 Audit.Failure (Audit.Forward Schema.Windows) Audit.Source  
  , Audit Audit.Tier2 Audit.Success (Audit.Forward Schema.Windows) Audit.Duration
  , Audit Audit.Tier2 Audit.Failure (Audit.Forward Schema.Windows) Audit.Duration
  , Audit Audit.Tier1 Audit.Success (Audit.Forward Schema.Windows) Audit.Source  
  , Audit Audit.Tier1 Audit.Failure (Audit.Forward Schema.Windows) Audit.Source  
  , Audit Audit.Tier1 Audit.Success (Audit.Forward Schema.Windows) Audit.Duration
  , Audit Audit.Tier1 Audit.Failure (Audit.Forward Schema.Windows) Audit.Duration

  ]

reports :: Array URI
reports =
  [ Audit Audit.Tier3 Audit.Success (Audit.Forward Schema.Audit) Audit.Source  
  , Audit Audit.Tier3 Audit.Failure (Audit.Forward Schema.Audit) Audit.Source  
  , Audit Audit.Tier3 Audit.Success (Audit.Forward Schema.Audit) Audit.Duration
  , Audit Audit.Tier3 Audit.Failure (Audit.Forward Schema.Audit) Audit.Duration
  , Audit Audit.Tier2 Audit.Success (Audit.Forward Schema.Audit) Audit.Source  
  , Audit Audit.Tier2 Audit.Failure (Audit.Forward Schema.Audit) Audit.Source  
  , Audit Audit.Tier2 Audit.Success (Audit.Forward Schema.Audit) Audit.Duration
  , Audit Audit.Tier2 Audit.Failure (Audit.Forward Schema.Audit) Audit.Duration
  , Audit Audit.Tier1 Audit.Success (Audit.Forward Schema.Audit) Audit.Source  
  , Audit Audit.Tier1 Audit.Failure (Audit.Forward Schema.Audit) Audit.Source  
  , Audit Audit.Tier1 Audit.Success (Audit.Forward Schema.Audit) Audit.Duration
  , Audit Audit.Tier1 Audit.Failure (Audit.Forward Schema.Audit) Audit.Duration
  , Audit Audit.Tier3 Audit.Success (Audit.Forward Schema.Alert) Audit.Source  

  , Audit Audit.Tier3 Audit.Failure (Audit.Forward Schema.Alert) Audit.Source  
  , Audit Audit.Tier3 Audit.Success (Audit.Forward Schema.Alert) Audit.Duration
  , Audit Audit.Tier3 Audit.Failure (Audit.Forward Schema.Alert) Audit.Duration
  , Audit Audit.Tier2 Audit.Success (Audit.Forward Schema.Alert) Audit.Source  
  , Audit Audit.Tier2 Audit.Failure (Audit.Forward Schema.Alert) Audit.Source  
  , Audit Audit.Tier2 Audit.Success (Audit.Forward Schema.Alert) Audit.Duration
  , Audit Audit.Tier2 Audit.Failure (Audit.Forward Schema.Alert) Audit.Duration
  , Audit Audit.Tier1 Audit.Success (Audit.Forward Schema.Alert) Audit.Source  
  , Audit Audit.Tier1 Audit.Failure (Audit.Forward Schema.Alert) Audit.Source  
  , Audit Audit.Tier1 Audit.Success (Audit.Forward Schema.Alert) Audit.Duration
  , Audit Audit.Tier1 Audit.Failure (Audit.Forward Schema.Alert) Audit.Duration

  , Audit Audit.Tier3 Audit.Failure (Audit.Forward Schema.Flow) Audit.Source  
  , Audit Audit.Tier3 Audit.Success (Audit.Forward Schema.Flow) Audit.Duration
  , Audit Audit.Tier3 Audit.Failure (Audit.Forward Schema.Flow) Audit.Duration
  , Audit Audit.Tier2 Audit.Success (Audit.Forward Schema.Flow) Audit.Source  
  , Audit Audit.Tier2 Audit.Failure (Audit.Forward Schema.Flow) Audit.Source  
  , Audit Audit.Tier2 Audit.Success (Audit.Forward Schema.Flow) Audit.Duration
  , Audit Audit.Tier2 Audit.Failure (Audit.Forward Schema.Flow) Audit.Duration
  , Audit Audit.Tier1 Audit.Success (Audit.Forward Schema.Flow) Audit.Source  
  , Audit Audit.Tier1 Audit.Failure (Audit.Forward Schema.Flow) Audit.Source  
  , Audit Audit.Tier1 Audit.Success (Audit.Forward Schema.Flow) Audit.Duration
  , Audit Audit.Tier1 Audit.Failure (Audit.Forward Schema.Flow) Audit.Duration

  , Audit Audit.Tier3 Audit.Failure (Audit.Forward Schema.Statistics) Audit.Source  
  , Audit Audit.Tier3 Audit.Success (Audit.Forward Schema.Statistics) Audit.Duration
  , Audit Audit.Tier3 Audit.Failure (Audit.Forward Schema.Statistics) Audit.Duration
  , Audit Audit.Tier2 Audit.Success (Audit.Forward Schema.Statistics) Audit.Source  
  , Audit Audit.Tier2 Audit.Failure (Audit.Forward Schema.Statistics) Audit.Source  
  , Audit Audit.Tier2 Audit.Success (Audit.Forward Schema.Statistics) Audit.Duration
  , Audit Audit.Tier2 Audit.Failure (Audit.Forward Schema.Statistics) Audit.Duration
  , Audit Audit.Tier1 Audit.Success (Audit.Forward Schema.Statistics) Audit.Source  
  , Audit Audit.Tier1 Audit.Failure (Audit.Forward Schema.Statistics) Audit.Source  
  , Audit Audit.Tier1 Audit.Success (Audit.Forward Schema.Statistics) Audit.Duration
  , Audit Audit.Tier1 Audit.Failure (Audit.Forward Schema.Statistics) Audit.Duration

  , Audit Audit.Tier3 Audit.Failure (Audit.Forward Schema.Linux) Audit.Source  
  , Audit Audit.Tier3 Audit.Success (Audit.Forward Schema.Linux) Audit.Duration
  , Audit Audit.Tier3 Audit.Failure (Audit.Forward Schema.Linux) Audit.Duration
  , Audit Audit.Tier2 Audit.Success (Audit.Forward Schema.Linux) Audit.Source  
  , Audit Audit.Tier2 Audit.Failure (Audit.Forward Schema.Linux) Audit.Source  
  , Audit Audit.Tier2 Audit.Success (Audit.Forward Schema.Linux) Audit.Duration
  , Audit Audit.Tier2 Audit.Failure (Audit.Forward Schema.Linux) Audit.Duration
  , Audit Audit.Tier1 Audit.Success (Audit.Forward Schema.Linux) Audit.Source  
  , Audit Audit.Tier1 Audit.Failure (Audit.Forward Schema.Linux) Audit.Source  
  , Audit Audit.Tier1 Audit.Success (Audit.Forward Schema.Linux) Audit.Duration
  , Audit Audit.Tier1 Audit.Failure (Audit.Forward Schema.Linux) Audit.Duration

  , Audit Audit.Tier3 Audit.Failure (Audit.Forward Schema.Windows) Audit.Source  
  , Audit Audit.Tier3 Audit.Success (Audit.Forward Schema.Windows) Audit.Duration
  , Audit Audit.Tier3 Audit.Failure (Audit.Forward Schema.Windows) Audit.Duration
  , Audit Audit.Tier2 Audit.Success (Audit.Forward Schema.Windows) Audit.Source  
  , Audit Audit.Tier2 Audit.Failure (Audit.Forward Schema.Windows) Audit.Source  
  , Audit Audit.Tier2 Audit.Success (Audit.Forward Schema.Windows) Audit.Duration
  , Audit Audit.Tier2 Audit.Failure (Audit.Forward Schema.Windows) Audit.Duration
  , Audit Audit.Tier1 Audit.Success (Audit.Forward Schema.Windows) Audit.Source  
  , Audit Audit.Tier1 Audit.Failure (Audit.Forward Schema.Windows) Audit.Source  
  , Audit Audit.Tier1 Audit.Success (Audit.Forward Schema.Windows) Audit.Duration
  , Audit Audit.Tier1 Audit.Failure (Audit.Forward Schema.Windows) Audit.Duration

  ]

uri :: URI -> String
uri  (Audit Audit.Tier3 Audit.Success (Audit.Forward Schema.Audit) Audit.Source)   = "/report/audit/tier3/success/forward-audit/source"
uri  (Audit Audit.Tier3 Audit.Failure (Audit.Forward Schema.Audit) Audit.Source)   = "/report/audit/tier3/failure/forward-audit/source"
uri  (Audit Audit.Tier3 Audit.Success (Audit.Forward Schema.Audit) Audit.Duration) = "/report/audit/tier3/success/forward-audit/duration"
uri  (Audit Audit.Tier3 Audit.Failure (Audit.Forward Schema.Audit) Audit.Duration) = "/report/audit/tier3/failure/forward-audit/duration"
uri  (Audit Audit.Tier2 Audit.Success (Audit.Forward Schema.Audit) Audit.Source)   = "/report/audit/tier2/success/forward-audit/source"
uri  (Audit Audit.Tier2 Audit.Failure (Audit.Forward Schema.Audit) Audit.Source)   = "/report/audit/tier2/failure/forward-audit/source"
uri  (Audit Audit.Tier2 Audit.Success (Audit.Forward Schema.Audit) Audit.Duration) = "/report/audit/tier2/success/forward-audit/duration"
uri  (Audit Audit.Tier2 Audit.Failure (Audit.Forward Schema.Audit) Audit.Duration) = "/report/audit/tier2/failure/forward-audit/duration"
uri  (Audit Audit.Tier1 Audit.Success (Audit.Forward Schema.Audit) Audit.Source)   = "/report/audit/tier1/success/forward-audit/source"
uri  (Audit Audit.Tier1 Audit.Failure (Audit.Forward Schema.Audit) Audit.Source)   = "/report/audit/tier1/failure/forward-audit/source"
uri  (Audit Audit.Tier1 Audit.Success (Audit.Forward Schema.Audit) Audit.Duration) = "/report/audit/tier1/success/forward-audit/duration"
uri  (Audit Audit.Tier1 Audit.Failure (Audit.Forward Schema.Audit) Audit.Duration) = "/report/audit/tier1/failure/forward-audit/duration"
uri  (Audit Audit.Tier3 Audit.Success (Audit.Forward Schema.Alert) Audit.Source)   = "/report/audit/tier3/success/forward-alert/source"
uri  (Audit Audit.Tier3 Audit.Failure (Audit.Forward Schema.Alert) Audit.Source)   = "/report/audit/tier3/failure/forward-alert/source"
uri  (Audit Audit.Tier3 Audit.Success (Audit.Forward Schema.Alert) Audit.Duration) = "/report/audit/tier3/success/forward-alert/duration"
uri  (Audit Audit.Tier3 Audit.Failure (Audit.Forward Schema.Alert) Audit.Duration) = "/report/audit/tier3/failure/forward-alert/duration"
uri  (Audit Audit.Tier2 Audit.Success (Audit.Forward Schema.Alert) Audit.Source)   = "/report/audit/tier2/success/forward-alert/source"
uri  (Audit Audit.Tier2 Audit.Failure (Audit.Forward Schema.Alert) Audit.Source)   = "/report/audit/tier2/failure/forward-alert/source"
uri  (Audit Audit.Tier2 Audit.Success (Audit.Forward Schema.Alert) Audit.Duration) = "/report/audit/tier2/success/forward-alert/duration"
uri  (Audit Audit.Tier2 Audit.Failure (Audit.Forward Schema.Alert) Audit.Duration) = "/report/audit/tier2/failure/forward-alert/duration"
uri  (Audit Audit.Tier1 Audit.Success (Audit.Forward Schema.Alert) Audit.Source)   = "/report/audit/tier1/success/forward-alert/source"
uri  (Audit Audit.Tier1 Audit.Failure (Audit.Forward Schema.Alert) Audit.Source)   = "/report/audit/tier1/failure/forward-alert/source"
uri  (Audit Audit.Tier1 Audit.Success (Audit.Forward Schema.Alert) Audit.Duration) = "/report/audit/tier1/success/forward-alert/duration"
uri  (Audit Audit.Tier1 Audit.Failure (Audit.Forward Schema.Alert) Audit.Duration) = "/report/audit/tier1/failure/forward-alert/duration"
uri  (Audit Audit.Tier3 Audit.Success (Audit.Forward Schema.Flow) Audit.Source)   = "/report/audit/tier3/success/forward-flow/source"
uri  (Audit Audit.Tier3 Audit.Failure (Audit.Forward Schema.Flow) Audit.Source)   = "/report/audit/tier3/failure/forward-flow/source"
uri  (Audit Audit.Tier3 Audit.Success (Audit.Forward Schema.Flow) Audit.Duration) = "/report/audit/tier3/success/forward-flow/duration"
uri  (Audit Audit.Tier3 Audit.Failure (Audit.Forward Schema.Flow) Audit.Duration) = "/report/audit/tier3/failure/forward-flow/duration"
uri  (Audit Audit.Tier2 Audit.Success (Audit.Forward Schema.Flow) Audit.Source)   = "/report/audit/tier2/success/forward-flow/source"
uri  (Audit Audit.Tier2 Audit.Failure (Audit.Forward Schema.Flow) Audit.Source)   = "/report/audit/tier2/failure/forward-flow/source"
uri  (Audit Audit.Tier2 Audit.Success (Audit.Forward Schema.Flow) Audit.Duration) = "/report/audit/tier2/success/forward-flow/duration"
uri  (Audit Audit.Tier2 Audit.Failure (Audit.Forward Schema.Flow) Audit.Duration) = "/report/audit/tier2/failure/forward-flow/duration"
uri  (Audit Audit.Tier1 Audit.Success (Audit.Forward Schema.Flow) Audit.Source)   = "/report/audit/tier1/success/forward-flow/source"
uri  (Audit Audit.Tier1 Audit.Failure (Audit.Forward Schema.Flow) Audit.Source)   = "/report/audit/tier1/failure/forward-flow/source"
uri  (Audit Audit.Tier1 Audit.Success (Audit.Forward Schema.Flow) Audit.Duration) = "/report/audit/tier1/success/forward-flow/duration"
uri  (Audit Audit.Tier1 Audit.Failure (Audit.Forward Schema.Flow) Audit.Duration) = "/report/audit/tier1/failure/forward-flow/duration"
uri  (Audit Audit.Tier3 Audit.Success (Audit.Forward Schema.Statistics) Audit.Source)   = "/report/audit/tier3/success/forward-statistics/source"
uri  (Audit Audit.Tier3 Audit.Failure (Audit.Forward Schema.Statistics) Audit.Source)   = "/report/audit/tier3/failure/forward-statistics/source"
uri  (Audit Audit.Tier3 Audit.Success (Audit.Forward Schema.Statistics) Audit.Duration) = "/report/audit/tier3/success/forward-statistics/duration"
uri  (Audit Audit.Tier3 Audit.Failure (Audit.Forward Schema.Statistics) Audit.Duration) = "/report/audit/tier3/failure/forward-statistics/duration"
uri  (Audit Audit.Tier2 Audit.Success (Audit.Forward Schema.Statistics) Audit.Source)   = "/report/audit/tier2/success/forward-statistics/source"
uri  (Audit Audit.Tier2 Audit.Failure (Audit.Forward Schema.Statistics) Audit.Source)   = "/report/audit/tier2/failure/forward-statistics/source"
uri  (Audit Audit.Tier2 Audit.Success (Audit.Forward Schema.Statistics) Audit.Duration) = "/report/audit/tier2/success/forward-statistics/duration"
uri  (Audit Audit.Tier2 Audit.Failure (Audit.Forward Schema.Statistics) Audit.Duration) = "/report/audit/tier2/failure/forward-statistics/duration"
uri  (Audit Audit.Tier1 Audit.Success (Audit.Forward Schema.Statistics) Audit.Source)   = "/report/audit/tier1/success/forward-statistics/source"
uri  (Audit Audit.Tier1 Audit.Failure (Audit.Forward Schema.Statistics) Audit.Source)   = "/report/audit/tier1/failure/forward-statistics/source"
uri  (Audit Audit.Tier1 Audit.Success (Audit.Forward Schema.Statistics) Audit.Duration) = "/report/audit/tier1/success/forward-statistics/duration"
uri  (Audit Audit.Tier1 Audit.Failure (Audit.Forward Schema.Statistics) Audit.Duration) = "/report/audit/tier1/failure/forward-statistics/duration"
uri  (Audit Audit.Tier3 Audit.Success (Audit.Forward Schema.Linux) Audit.Source)   = "/report/audit/tier3/success/forward-linux/source"
uri  (Audit Audit.Tier3 Audit.Failure (Audit.Forward Schema.Linux) Audit.Source)   = "/report/audit/tier3/failure/forward-linux/source"
uri  (Audit Audit.Tier3 Audit.Success (Audit.Forward Schema.Linux) Audit.Duration) = "/report/audit/tier3/success/forward-linux/duration"
uri  (Audit Audit.Tier3 Audit.Failure (Audit.Forward Schema.Linux) Audit.Duration) = "/report/audit/tier3/failure/forward-linux/duration"
uri  (Audit Audit.Tier2 Audit.Success (Audit.Forward Schema.Linux) Audit.Source)   = "/report/audit/tier2/success/forward-linux/source"
uri  (Audit Audit.Tier2 Audit.Failure (Audit.Forward Schema.Linux) Audit.Source)   = "/report/audit/tier2/failure/forward-linux/source"
uri  (Audit Audit.Tier2 Audit.Success (Audit.Forward Schema.Linux) Audit.Duration) = "/report/audit/tier2/success/forward-linux/duration"
uri  (Audit Audit.Tier2 Audit.Failure (Audit.Forward Schema.Linux) Audit.Duration) = "/report/audit/tier2/failure/forward-linux/duration"
uri  (Audit Audit.Tier1 Audit.Success (Audit.Forward Schema.Linux) Audit.Source)   = "/report/audit/tier1/success/forward-linux/source"
uri  (Audit Audit.Tier1 Audit.Failure (Audit.Forward Schema.Linux) Audit.Source)   = "/report/audit/tier1/failure/forward-linux/source"
uri  (Audit Audit.Tier1 Audit.Success (Audit.Forward Schema.Linux) Audit.Duration) = "/report/audit/tier1/success/forward-linux/duration"
uri  (Audit Audit.Tier1 Audit.Failure (Audit.Forward Schema.Linux) Audit.Duration) = "/report/audit/tier1/failure/forward-linux/duration"
uri  (Audit Audit.Tier3 Audit.Success (Audit.Forward Schema.Windows) Audit.Source)   = "/report/audit/tier3/success/forward-windows/source"
uri  (Audit Audit.Tier3 Audit.Failure (Audit.Forward Schema.Windows) Audit.Source)   = "/report/audit/tier3/failure/forward-windows/source"
uri  (Audit Audit.Tier3 Audit.Success (Audit.Forward Schema.Windows) Audit.Duration) = "/report/audit/tier3/success/forward-windows/duration"
uri  (Audit Audit.Tier3 Audit.Failure (Audit.Forward Schema.Windows) Audit.Duration) = "/report/audit/tier3/failure/forward-windows/duration"
uri  (Audit Audit.Tier2 Audit.Success (Audit.Forward Schema.Windows) Audit.Source)   = "/report/audit/tier2/success/forward-windows/source"
uri  (Audit Audit.Tier2 Audit.Failure (Audit.Forward Schema.Windows) Audit.Source)   = "/report/audit/tier2/failure/forward-windows/source"
uri  (Audit Audit.Tier2 Audit.Success (Audit.Forward Schema.Windows) Audit.Duration) = "/report/audit/tier2/success/forward-windows/duration"
uri  (Audit Audit.Tier2 Audit.Failure (Audit.Forward Schema.Windows) Audit.Duration) = "/report/audit/tier2/failure/forward-windows/duration"
uri  (Audit Audit.Tier1 Audit.Success (Audit.Forward Schema.Windows) Audit.Source)   = "/report/audit/tier1/success/forward-windows/source"
uri  (Audit Audit.Tier1 Audit.Failure (Audit.Forward Schema.Windows) Audit.Source)   = "/report/audit/tier1/failure/forward-windows/source"
uri  (Audit Audit.Tier1 Audit.Success (Audit.Forward Schema.Windows) Audit.Duration) = "/report/audit/tier1/success/forward-windows/duration"
uri  (Audit Audit.Tier1 Audit.Failure (Audit.Forward Schema.Windows) Audit.Duration) = "/report/audit/tier1/failure/forward-windows/duration"
uri  (Audit Audit.Tier3 Audit.Success (Audit.Report Schema.Audit) Audit.Source)   = "/report/audit/tier3/success/report-audit/source"
uri  (Audit Audit.Tier3 Audit.Failure (Audit.Report Schema.Audit) Audit.Source)   = "/report/audit/tier3/failure/report-audit/source"
uri  (Audit Audit.Tier3 Audit.Success (Audit.Report Schema.Audit) Audit.Duration) = "/report/audit/tier3/success/report-audit/duration"
uri  (Audit Audit.Tier3 Audit.Failure (Audit.Report Schema.Audit) Audit.Duration) = "/report/audit/tier3/failure/report-audit/duration"
uri  (Audit Audit.Tier2 Audit.Success (Audit.Report Schema.Audit) Audit.Source)   = "/report/audit/tier2/success/report-audit/source"
uri  (Audit Audit.Tier2 Audit.Failure (Audit.Report Schema.Audit) Audit.Source)   = "/report/audit/tier2/failure/report-audit/source"
uri  (Audit Audit.Tier2 Audit.Success (Audit.Report Schema.Audit) Audit.Duration) = "/report/audit/tier2/success/report-audit/duration"
uri  (Audit Audit.Tier2 Audit.Failure (Audit.Report Schema.Audit) Audit.Duration) = "/report/audit/tier2/failure/report-audit/duration"
uri  (Audit Audit.Tier1 Audit.Success (Audit.Report Schema.Audit) Audit.Source)   = "/report/audit/tier1/success/report-audit/source"
uri  (Audit Audit.Tier1 Audit.Failure (Audit.Report Schema.Audit) Audit.Source)   = "/report/audit/tier1/failure/report-audit/source"
uri  (Audit Audit.Tier1 Audit.Success (Audit.Report Schema.Audit) Audit.Duration) = "/report/audit/tier1/success/report-audit/duration"
uri  (Audit Audit.Tier1 Audit.Failure (Audit.Report Schema.Audit) Audit.Duration) = "/report/audit/tier1/failure/report-audit/duration"
uri  (Audit Audit.Tier3 Audit.Success (Audit.Report Schema.Alert) Audit.Source)   = "/report/audit/tier3/success/report-alert/source"
uri  (Audit Audit.Tier3 Audit.Failure (Audit.Report Schema.Alert) Audit.Source)   = "/report/audit/tier3/failure/report-alert/source"
uri  (Audit Audit.Tier3 Audit.Success (Audit.Report Schema.Alert) Audit.Duration) = "/report/audit/tier3/success/report-alert/duration"
uri  (Audit Audit.Tier3 Audit.Failure (Audit.Report Schema.Alert) Audit.Duration) = "/report/audit/tier3/failure/report-alert/duration"
uri  (Audit Audit.Tier2 Audit.Success (Audit.Report Schema.Alert) Audit.Source)   = "/report/audit/tier2/success/report-alert/source"
uri  (Audit Audit.Tier2 Audit.Failure (Audit.Report Schema.Alert) Audit.Source)   = "/report/audit/tier2/failure/report-alert/source"
uri  (Audit Audit.Tier2 Audit.Success (Audit.Report Schema.Alert) Audit.Duration) = "/report/audit/tier2/success/report-alert/duration"
uri  (Audit Audit.Tier2 Audit.Failure (Audit.Report Schema.Alert) Audit.Duration) = "/report/audit/tier2/failure/report-alert/duration"
uri  (Audit Audit.Tier1 Audit.Success (Audit.Report Schema.Alert) Audit.Source)   = "/report/audit/tier1/success/report-alert/source"
uri  (Audit Audit.Tier1 Audit.Failure (Audit.Report Schema.Alert) Audit.Source)   = "/report/audit/tier1/failure/report-alert/source"
uri  (Audit Audit.Tier1 Audit.Success (Audit.Report Schema.Alert) Audit.Duration) = "/report/audit/tier1/success/report-alert/duration"
uri  (Audit Audit.Tier1 Audit.Failure (Audit.Report Schema.Alert) Audit.Duration) = "/report/audit/tier1/failure/report-alert/duration"
uri  (Audit Audit.Tier3 Audit.Success (Audit.Report Schema.Flow) Audit.Source)   = "/report/audit/tier3/success/report-flow/source"
uri  (Audit Audit.Tier3 Audit.Failure (Audit.Report Schema.Flow) Audit.Source)   = "/report/audit/tier3/failure/report-flow/source"
uri  (Audit Audit.Tier3 Audit.Success (Audit.Report Schema.Flow) Audit.Duration) = "/report/audit/tier3/success/report-flow/duration"
uri  (Audit Audit.Tier3 Audit.Failure (Audit.Report Schema.Flow) Audit.Duration) = "/report/audit/tier3/failure/report-flow/duration"
uri  (Audit Audit.Tier2 Audit.Success (Audit.Report Schema.Flow) Audit.Source)   = "/report/audit/tier2/success/report-flow/source"
uri  (Audit Audit.Tier2 Audit.Failure (Audit.Report Schema.Flow) Audit.Source)   = "/report/audit/tier2/failure/report-flow/source"
uri  (Audit Audit.Tier2 Audit.Success (Audit.Report Schema.Flow) Audit.Duration) = "/report/audit/tier2/success/report-flow/duration"
uri  (Audit Audit.Tier2 Audit.Failure (Audit.Report Schema.Flow) Audit.Duration) = "/report/audit/tier2/failure/report-flow/duration"
uri  (Audit Audit.Tier1 Audit.Success (Audit.Report Schema.Flow) Audit.Source)   = "/report/audit/tier1/success/report-flow/source"
uri  (Audit Audit.Tier1 Audit.Failure (Audit.Report Schema.Flow) Audit.Source)   = "/report/audit/tier1/failure/report-flow/source"
uri  (Audit Audit.Tier1 Audit.Success (Audit.Report Schema.Flow) Audit.Duration) = "/report/audit/tier1/success/report-flow/duration"
uri  (Audit Audit.Tier1 Audit.Failure (Audit.Report Schema.Flow) Audit.Duration) = "/report/audit/tier1/failure/report-flow/duration"
uri  (Audit Audit.Tier3 Audit.Success (Audit.Report Schema.Statistics) Audit.Source)   = "/report/audit/tier3/success/report-statistics/source"
uri  (Audit Audit.Tier3 Audit.Failure (Audit.Report Schema.Statistics) Audit.Source)   = "/report/audit/tier3/failure/report-statistics/source"
uri  (Audit Audit.Tier3 Audit.Success (Audit.Report Schema.Statistics) Audit.Duration) = "/report/audit/tier3/success/report-statistics/duration"
uri  (Audit Audit.Tier3 Audit.Failure (Audit.Report Schema.Statistics) Audit.Duration) = "/report/audit/tier3/failure/report-statistics/duration"
uri  (Audit Audit.Tier2 Audit.Success (Audit.Report Schema.Statistics) Audit.Source)   = "/report/audit/tier2/success/report-statistics/source"
uri  (Audit Audit.Tier2 Audit.Failure (Audit.Report Schema.Statistics) Audit.Source)   = "/report/audit/tier2/failure/report-statistics/source"
uri  (Audit Audit.Tier2 Audit.Success (Audit.Report Schema.Statistics) Audit.Duration) = "/report/audit/tier2/success/report-statistics/duration"
uri  (Audit Audit.Tier2 Audit.Failure (Audit.Report Schema.Statistics) Audit.Duration) = "/report/audit/tier2/failure/report-statistics/duration"
uri  (Audit Audit.Tier1 Audit.Success (Audit.Report Schema.Statistics) Audit.Source)   = "/report/audit/tier1/success/report-statistics/source"
uri  (Audit Audit.Tier1 Audit.Failure (Audit.Report Schema.Statistics) Audit.Source)   = "/report/audit/tier1/failure/report-statistics/source"
uri  (Audit Audit.Tier1 Audit.Success (Audit.Report Schema.Statistics) Audit.Duration) = "/report/audit/tier1/success/report-statistics/duration"
uri  (Audit Audit.Tier1 Audit.Failure (Audit.Report Schema.Statistics) Audit.Duration) = "/report/audit/tier1/failure/report-statistics/duration"
uri  (Audit Audit.Tier3 Audit.Success (Audit.Report Schema.Linux) Audit.Source)   = "/report/audit/tier3/success/report-linux/source"
uri  (Audit Audit.Tier3 Audit.Failure (Audit.Report Schema.Linux) Audit.Source)   = "/report/audit/tier3/failure/report-linux/source"
uri  (Audit Audit.Tier3 Audit.Success (Audit.Report Schema.Linux) Audit.Duration) = "/report/audit/tier3/success/report-linux/duration"
uri  (Audit Audit.Tier3 Audit.Failure (Audit.Report Schema.Linux) Audit.Duration) = "/report/audit/tier3/failure/report-linux/duration"
uri  (Audit Audit.Tier2 Audit.Success (Audit.Report Schema.Linux) Audit.Source)   = "/report/audit/tier2/success/report-linux/source"
uri  (Audit Audit.Tier2 Audit.Failure (Audit.Report Schema.Linux) Audit.Source)   = "/report/audit/tier2/failure/report-linux/source"
uri  (Audit Audit.Tier2 Audit.Success (Audit.Report Schema.Linux) Audit.Duration) = "/report/audit/tier2/success/report-linux/duration"
uri  (Audit Audit.Tier2 Audit.Failure (Audit.Report Schema.Linux) Audit.Duration) = "/report/audit/tier2/failure/report-linux/duration"
uri  (Audit Audit.Tier1 Audit.Success (Audit.Report Schema.Linux) Audit.Source)   = "/report/audit/tier1/success/report-linux/source"
uri  (Audit Audit.Tier1 Audit.Failure (Audit.Report Schema.Linux) Audit.Source)   = "/report/audit/tier1/failure/report-linux/source"
uri  (Audit Audit.Tier1 Audit.Success (Audit.Report Schema.Linux) Audit.Duration) = "/report/audit/tier1/success/report-linux/duration"
uri  (Audit Audit.Tier1 Audit.Failure (Audit.Report Schema.Linux) Audit.Duration) = "/report/audit/tier1/failure/report-linux/duration"
uri  (Audit Audit.Tier3 Audit.Success (Audit.Report Schema.Windows) Audit.Source)   = "/report/audit/tier3/success/report-windows/source"
uri  (Audit Audit.Tier3 Audit.Failure (Audit.Report Schema.Windows) Audit.Source)   = "/report/audit/tier3/failure/report-windows/source"
uri  (Audit Audit.Tier3 Audit.Success (Audit.Report Schema.Windows) Audit.Duration) = "/report/audit/tier3/success/report-windows/duration"
uri  (Audit Audit.Tier3 Audit.Failure (Audit.Report Schema.Windows) Audit.Duration) = "/report/audit/tier3/failure/report-windows/duration"
uri  (Audit Audit.Tier2 Audit.Success (Audit.Report Schema.Windows) Audit.Source)   = "/report/audit/tier2/success/report-windows/source"
uri  (Audit Audit.Tier2 Audit.Failure (Audit.Report Schema.Windows) Audit.Source)   = "/report/audit/tier2/failure/report-windows/source"
uri  (Audit Audit.Tier2 Audit.Success (Audit.Report Schema.Windows) Audit.Duration) = "/report/audit/tier2/success/report-windows/duration"
uri  (Audit Audit.Tier2 Audit.Failure (Audit.Report Schema.Windows) Audit.Duration) = "/report/audit/tier2/failure/report-windows/duration"
uri  (Audit Audit.Tier1 Audit.Success (Audit.Report Schema.Windows) Audit.Source)   = "/report/audit/tier1/success/report-windows/source"
uri  (Audit Audit.Tier1 Audit.Failure (Audit.Report Schema.Windows) Audit.Source)   = "/report/audit/tier1/failure/report-windows/source"
uri  (Audit Audit.Tier1 Audit.Success (Audit.Report Schema.Windows) Audit.Duration) = "/report/audit/tier1/success/report-windows/duration"
uri  (Audit Audit.Tier3 Audit.Success Audit.Risk Audit.Source)   = "/report/audit/tier3/success/anomalous/source"
uri  (Audit Audit.Tier3 Audit.Failure Audit.Risk Audit.Source)   = "/report/audit/tier3/failure/risk/source"
uri  (Audit Audit.Tier3 Audit.Success Audit.Risk Audit.Duration) = "/report/audit/tier3/success/risk/duration"
uri  (Audit Audit.Tier3 Audit.Failure Audit.Risk Audit.Duration) = "/report/audit/tier3/failure/risk/duration"
uri  (Audit Audit.Tier2 Audit.Success Audit.Risk Audit.Source)   = "/report/audit/tier2/success/risk/source"
uri  (Audit Audit.Tier2 Audit.Failure Audit.Risk Audit.Source)   = "/report/audit/tier2/failure/risk/source"
uri  (Audit Audit.Tier2 Audit.Success Audit.Risk Audit.Duration) = "/report/audit/tier2/success/risk/duration"
uri  (Audit Audit.Tier2 Audit.Failure Audit.Risk Audit.Duration) = "/report/audit/tier2/failure/risk/duration"
uri  (Audit Audit.Tier1 Audit.Success Audit.Risk Audit.Source)   = "/report/audit/tier1/success/risk/source"
uri  (Audit Audit.Tier1 Audit.Failure Audit.Risk Audit.Source)   = "/report/audit/tier1/failure/risk/source"
uri  (Audit Audit.Tier1 Audit.Success Audit.Risk Audit.Duration) = "/report/audit/tier1/success/risk/duration"
uri  (Audit Audit.Tier1 Audit.Failure Audit.Risk Audit.Duration) = "/report/audit/tier1/failure/risk/duration"
uri  (Audit Audit.Tier1 Audit.Failure (Audit.Report Schema.Windows) Audit.Duration) = "/report/audit/tier1/failure/report-windows/duration"
uri  (Audit Audit.Tier3 Audit.Success Audit.Anomalous Audit.Source)   = "/report/audit/tier3/success/anomalous/source"
uri  (Audit Audit.Tier3 Audit.Failure Audit.Anomalous Audit.Source)   = "/report/audit/tier3/failure/anomalous/source"
uri  (Audit Audit.Tier3 Audit.Success Audit.Anomalous Audit.Duration) = "/report/audit/tier3/success/anomalous/duration"
uri  (Audit Audit.Tier3 Audit.Failure Audit.Anomalous Audit.Duration) = "/report/audit/tier3/failure/anomalous/duration"
uri  (Audit Audit.Tier2 Audit.Success Audit.Anomalous Audit.Source)   = "/report/audit/tier2/success/anomalous/source"
uri  (Audit Audit.Tier2 Audit.Failure Audit.Anomalous Audit.Source)   = "/report/audit/tier2/failure/anomalous/source"
uri  (Audit Audit.Tier2 Audit.Success Audit.Anomalous Audit.Duration) = "/report/audit/tier2/success/anomalous/duration"
uri  (Audit Audit.Tier2 Audit.Failure Audit.Anomalous Audit.Duration) = "/report/audit/tier2/failure/anomalous/duration"
uri  (Audit Audit.Tier1 Audit.Success Audit.Anomalous Audit.Source)   = "/report/audit/tier1/success/anomalous/source"
uri  (Audit Audit.Tier1 Audit.Failure Audit.Anomalous Audit.Source)   = "/report/audit/tier1/failure/anomalous/source"
uri  (Audit Audit.Tier1 Audit.Success Audit.Anomalous Audit.Duration) = "/report/audit/tier1/success/anomalous/duration"
uri  (Audit Audit.Tier1 Audit.Failure Audit.Anomalous Audit.Duration) = "/report/audit/tier1/failure/anomalous/duration"
