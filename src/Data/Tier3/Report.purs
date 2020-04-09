module Data.Tier3.Report
  ( Report(..)
  , all
  , uri 
  ) where

import Data.Audit as Audit

data Report = Audit Audit.EventCategory Audit.EventType Audit.ReportType

all :: Array Report
all =
  [ Audit Audit.Tier3 Audit.Success Audit.Source  
  , Audit Audit.Tier3 Audit.Failure Audit.Source  
  , Audit Audit.Tier3 Audit.Success Audit.Duration
  , Audit Audit.Tier3 Audit.Failure Audit.Duration
  , Audit Audit.Tier2 Audit.Success Audit.Source  
  , Audit Audit.Tier2 Audit.Failure Audit.Source  
  , Audit Audit.Tier2 Audit.Success Audit.Duration
  , Audit Audit.Tier2 Audit.Failure Audit.Duration
  , Audit Audit.Tier1 Audit.Success Audit.Source  
  , Audit Audit.Tier1 Audit.Failure Audit.Source  
  , Audit Audit.Tier1 Audit.Success Audit.Duration
  , Audit Audit.Tier1 Audit.Failure Audit.Duration
  ]

uri :: Report -> String
uri  (Audit Audit.Tier3 Audit.Success Audit.Source)   = "/report/audit/tier3/success/source"
uri  (Audit Audit.Tier3 Audit.Failure Audit.Source)   = "/report/audit/tier3/failure/source"
uri  (Audit Audit.Tier3 Audit.Success Audit.Duration) = "/report/audit/tier3/success/duration"
uri  (Audit Audit.Tier3 Audit.Failure Audit.Duration) = "/report/audit/tier3/failure/duration"
uri  (Audit Audit.Tier2 Audit.Success Audit.Source)   = "/report/audit/tier2/success/source"
uri  (Audit Audit.Tier2 Audit.Failure Audit.Source)   = "/report/audit/tier2/failure/source"
uri  (Audit Audit.Tier2 Audit.Success Audit.Duration) = "/report/audit/tier2/success/duration"
uri  (Audit Audit.Tier2 Audit.Failure Audit.Duration) = "/report/audit/tier2/failure/duration"
uri  (Audit Audit.Tier1 Audit.Success Audit.Source)   = "/report/audit/tier1/success/source"
uri  (Audit Audit.Tier1 Audit.Failure Audit.Source)   = "/report/audit/tier1/failure/source"
uri  (Audit Audit.Tier1 Audit.Success Audit.Duration) = "/report/audit/tier1/success/duration"
uri  (Audit Audit.Tier1 Audit.Failure Audit.Duration) = "/report/audit/tier1/failure/duration"