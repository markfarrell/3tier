module Data.Event
  ( Event
  , Duration
  , Port
  ) where

import FFI.Date (Date)

import Data.IPv4 (IPv4)

type Duration = Int

type Port = Int

{--
  | # [PROJECT-01/RELEASE-01/ISSUE-01](https://github.com/markfarrell/3tier/issues/1)
  | ## Data.Alert.Event
  | 
  |   data Event = Data.Event Alert.EventCategory Alert.EventType Alert.EventID Alert.EventURI
  |
  | ## Data.Audit.Event
  |
  |   data Event = Data.Event Audit.EventCategory Audit.EventType Audit.EventID Audit.EventURI
  |
  | ## Data.Traffic.Event
  |
  |   data Event = Data.Event Traffic.EventCategory Traffic.EventType Traffic.EventID Traffic.EventURI
  |
  | ## Data.Report.Event
  |
  |   data Event = Data.Event Report.EventCategory Report.EventType Report.EventID Report.EventURI
  |
  | ## Data.Linux.Event
  |
  |   data Event = Data.Event Linux.EventCategory Linux.EventType Linux.EventID Linux.EventURI
  |
  | ## Data.Windows.Event
  |
  |   data Event = Data.Event Windows.EventCategory Windows.EventType Windows.EventID Windows.EventURI
  |
  | ...
--}
data Event a b c d = Event
  { eventCategory :: a
  , eventType     :: b
  , eventID       :: c
  , eventURI      :: d
  , startTime     :: Date
  , duration      :: Duration
  , endTime       :: Date
  , ip            :: IPv4
  , port          :: Port
  } 
