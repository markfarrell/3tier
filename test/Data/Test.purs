module Test.Data.Test
  ( Event(..)
  , EventCategory(..)
  , EventType(..)
  , EventID(..)
  , EventURI
  ) where

import Prelude

import Data.Foldable (intercalate)

import Data.Event (Time) as Event

data EventCategory = Tier1 | Tier2 | Tier3

data EventType = Single | Replication | Failover

data EventID = Forward | Report 

type EventURI = Unit

{-- | Analogous to Data.{Alert,Audit,Flow,Report,Linux,Windows,...}.Event --}
data Event = Event
  { eventCategory :: EventCategory
  , eventType     :: EventType
  , eventID       :: EventID
  , eventURI      :: EventURI
  , eventTime     :: Event.Time
  }

instance showEventTest :: Show Event where
  show (Event test) = intercalate " " columns
    where
      columns = 
        [ h1 $ "TEST"
        , h2 $ show test.eventCategory
        , h3 $ show test.eventType
        , h4 $ show test.eventID
        , p  $ show test.eventTime
        ]
      h1         = bold <<< fgGreen
      h2         = bold <<< fgMagenta
      h3         = bold <<< fgCyan
      h4         = bold <<< fgYellow
      p          = dim  <<< fgWhite
      square     = \x ->  (fgGreen "[") <> x <>  (fgGreen "]")
      round      = \x ->  (fgGreen "(") <> x <>  (fgGreen ")") 
      dim        = \x -> "\x1b[2m" <> x <> "\x1b[0m" 
      bold       = \x -> "\x1b[1m" <> x <> "\x1b[0m" 
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
  show Single      = "SINGLE"
  show Replication = "REPLICATION"
  show Failover    = "FAILOVER"

instance showEventIDTest :: Show EventID where
  show Forward = "FORWARD"
  show Report  = "REPORT"
