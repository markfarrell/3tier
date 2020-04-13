module Test.Data.Test
  ( Event(..)
  , EventCategory(..)
  , EventType(..)
  , EventID(..)
  , EventURI
  ) where

import Prelude

import Data.Foldable (intercalate)

import FFI.Date (Date)

import Data.IPv4 (IPv4)

data EventCategory = Tier1 | Tier2 | Tier3

data EventType = Local | Replication | Failover

data EventID = Forward | Report 

type EventURI = Unit

{-- | Analogous to Data.{Alert,Audit,Flow,Report,Linux,Windows,...}.Event --}
data Event = Event
  { eventCategory :: EventCategory
  , eventType     :: EventType
  , eventID       :: EventID
  , eventURI      :: EventURI
  , startTime     :: Date
  , duration      :: Int
  , endTime       :: Date
  , sIP           :: IPv4
  , sPort         :: Int 
  }

instance showEventTest :: Show Event where
  show (Event test) = intercalate " " columns
    where
      columns = 
        ["[TEST]"
        , show test.eventCategory
        , show test.eventType
        , show test.eventID
        , show test.startTime
        , show test.duration
        , show test.endTime
        ]
  
instance showEventCategoryTest :: Show EventCategory where
  show Tier1 = "TIER-01"
  show Tier2 = "TIER-02"
  show Tier3 = "TIER-03"

instance showEventTypeTest :: Show EventType where
  show Local       = "LOCAL"
  show Replication = "REPLICATION"
  show Failover    = "FAILOVER"

instance showEventIDTest :: Show EventID where
  show Forward = "FORWARD"
  show Report  = "REPORT"
