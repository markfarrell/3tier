module Report
  ( Record(..)
  , Report(..)
  , reports
  , uri
  ) where

import Prelude

import Audit as Audit

data Report = Audit Audit.EventCategory Audit.EventType Audit.ReportType

data Record = Record
  { min                 :: Number
  , max                 :: Number
  , sum                 :: Number
  , total               :: Number
  , average             :: Number
  , variance            :: Number 
  }

instance eqRecord :: Eq Record where
  eq (Record x) (Record y) = x == y

reports :: Array Report
reports =
  [ Audit Audit.DatabaseRequest Audit.Success Audit.Sources  
  , Audit Audit.DatabaseRequest Audit.Failure Audit.Sources  
  , Audit Audit.DatabaseRequest Audit.Success Audit.Durations
  , Audit Audit.DatabaseRequest Audit.Failure Audit.Durations
  , Audit Audit.ResourceRequest Audit.Success Audit.Sources  
  , Audit Audit.ResourceRequest Audit.Failure Audit.Sources  
  , Audit Audit.ResourceRequest Audit.Success Audit.Durations
  , Audit Audit.ResourceRequest Audit.Failure Audit.Durations
  , Audit Audit.RoutingRequest Audit.Success Audit.Sources   
  , Audit Audit.RoutingRequest Audit.Failure Audit.Sources   
  , Audit Audit.RoutingRequest Audit.Success Audit.Durations 
  , Audit Audit.RoutingRequest Audit.Failure Audit.Durations 
  ]

uri :: Report -> String
uri  (Audit Audit.DatabaseRequest Audit.Success Audit.Sources)   = "/report/audit/database-request/success/sources"
uri  (Audit Audit.DatabaseRequest Audit.Failure Audit.Sources)   = "/report/audit/database-request/success/sources" 
uri  (Audit Audit.DatabaseRequest Audit.Success Audit.Durations) = "/report/audit/database-request/success/durations"
uri  (Audit Audit.DatabaseRequest Audit.Failure Audit.Durations) = "/report/audit/database-request/success/durations"
uri  (Audit Audit.ResourceRequest Audit.Success Audit.Sources)   = "/report/audit/resource-request/success/sources"
uri  (Audit Audit.ResourceRequest Audit.Failure Audit.Sources)   = "/report/audit/resource-request/success/sources"
uri  (Audit Audit.ResourceRequest Audit.Success Audit.Durations) = "/report/audit/resource-request/success/durations"
uri  (Audit Audit.ResourceRequest Audit.Failure Audit.Durations) = "/report/audit/resource-request/success/durations"
uri  (Audit Audit.RoutingRequest Audit.Success Audit.Sources)    = "/report/audit/routing-request/success/sources" 
uri  (Audit Audit.RoutingRequest Audit.Failure Audit.Sources)    = "/report/audit/routing-request/success/sources"
uri  (Audit Audit.RoutingRequest Audit.Success Audit.Durations)  = "/report/audit/routing-request/success/durations"
uri  (Audit Audit.RoutingRequest Audit.Failure Audit.Durations)  = "/report/audit/routing-request/success/durations"
