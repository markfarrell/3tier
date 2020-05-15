module Test.Data.Test
  ( Event(..)
  , EventCategory(..)
  , EventType(..)
  , EventID(..)
  ) where

import Prelude

import Data.Foldable (intercalate)

import Data.Event as Event

data EventCategory = Tier1 | Tier2 | Tier3

data EventType = Single | Replication | Failover

data EventID = Forward | Report 

{-- | Analogous to Data.{Alert,Audit,Flow,Report,Linux,Windows,...}.Event --}
data Event = Event
  { eventCategory :: EventCategory
  , eventType     :: EventType
  , eventID       :: EventID
  , eventTime     :: Event.EventTime
  }

instance showEventTest :: Show Event where
  show (Event test) = intercalate " " columns
    where
      columns = 
        [ dim $ fgGreen "    >"
        , dim $ fgMagenta (show test.eventCategory)
        , dim $ fgCyan    (show test.eventType)
        , dim $ fgYellow  (show test.eventID)
        , dim $ fgWhite $ round (case test.eventTime of (Event.EventTime x) -> (show $ x.duration / 1000) <> "s")
        ]
      h1         = dim <<< fgGreen
      h2         = dim <<< fgMagenta
      h3         = dim <<< fgCyan
      h4         = dim <<< fgYellow
      p          = dim  <<< fgWhite
      round      = \x ->  "(" <> x <> ")" 
      dim        = \x -> "\x1b[2m" <> x <> "\x1b[0m" 
      fgCyan     = \x -> "\x1b[36m" <> x <> "\x1b[0m" 
      fgGreen    = \x -> "\x1b[32m" <> x <> "\x1b[0m" 
      fgYellow   = \x -> "\x1b[33m" <> x <> "\x1b[0m" 
      fgWhite    = \x -> "\x1b[37m" <> x <> "\x1b[0m" 
      fgMagenta  = \x -> "\x1b[35m" <> x <> "\x1b[0m" 
      underline  = \x -> "\x1b[4m" <> x <> "\x1b[0m" 
      {-- Reference: https://stackoverflow.com/questions/9781218/how-to-change-node-jss-console-font-color --}
  
instance showEventCategoryTest :: Show EventCategory where
  show Tier1 = "TIER-01"
  show Tier2 = "TIER-02"
  show Tier3 = "TIER-03"

instance showEventTypeTest :: Show EventType where
  show Single      = "SINGLE     "
  show Replication = "REPLICATION"
  show Failover    = "FAILOVER   "

instance showEventIDTest :: Show EventID where
  show Forward = "FORWARD"
  show Report  = "REPORT "
