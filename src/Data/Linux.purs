module Data.Linux
 ( EventCategory(..)
 , EventType(..)
 , EventID
 , EventURI
 , Event(..)
 , eventCategories
 , eventTypes
 ) where

import Prelude

import FFI.JSON as JSON
import FFI.UUID (UUID)

import Data.Event as Event

import Unsafe.Coerce (unsafeCoerce)

data EventCategory =
    DaemonStart
  | ConfigChange
  | SystemBoot
  | SystemRunLevel
  | ServiceStart
  | NetfilterCfg
  | Syscall
  | Proctitle
  | ServiceStop
  | UserStart
  | UserCmd
  | UserEnd
  | UserLogin
  | UserAuth
  | UserAcct
  | CredAcq
  | CredDisp
  | CredRefr
  | AnomPromisc
  | Login

data EventType = Success | Failure

type EventID = Int

type EventURI = UUID

data Event = Event
  { eventCategory :: EventCategory
  , eventType     :: EventType
  , eventID       :: EventID
  , eventURI      :: EventURI
  , eventSource   :: Event.EventSource
  , eventTime     :: Event.EventTime
  }

instance showEventLinux :: Show Event where
  show = uri

instance showEventTypeLinux :: Show EventType where
  show Success = "SUCCESS"
  show Failure = "FAILURE"

instance showEventCategoryLinux :: Show EventCategory where
  show DaemonStart    = "DAEMON-START"
  show ConfigChange   = "CONFIG-CHANGE"
  show SystemBoot     = "SYSTEM-BOOT"
  show SystemRunLevel = "SYSTEM-RUN-LEVEL"
  show ServiceStart   = "SERViCE-START"
  show NetfilterCfg   = "NETFILTER-CFG"
  show Syscall        = "SYSCALL"
  show Proctitle      = "PROCTITLE"
  show ServiceStop    = "SERVICE-STOP"
  show UserStart      = "USER-START"
  show UserCmd        = "USER-CMD"
  show UserEnd        = "USER-END"
  show UserLogin      = "USER-LOGIN"
  show UserAuth       = "USER-AUTH"
  show UserAcct       = "USER-ACCT"
  show CredAcq        = "CRED-ACQ"
  show CredDisp       = "CRED-DISP"
  show CredRefr       = "CRED-REFR"
  show AnomPromisc    = "ANOM-PROMISCUOUS"
  show Login          = "LOGIN"

instance eqEventCategoryLinux :: Eq EventCategory where
  eq DaemonStart    DaemonStart    = true
  eq ConfigChange   ConfigChange   = true 
  eq SystemBoot     SystemBoot     = true
  eq SystemRunLevel SystemRunLevel = true
  eq ServiceStart   ServiceStart   = true
  eq NetfilterCfg   NetfilterCfg   = true
  eq Syscall        Syscall        = true
  eq Proctitle      Proctitle      = true
  eq ServiceStop    ServiceStop    = true
  eq UserStart      UserStart      = true
  eq UserCmd        UserCmd        = true
  eq UserEnd        UserEnd        = true
  eq UserLogin      UserLogin      = true
  eq UserAuth       UserAuth       = true
  eq UserAcct       UserAcct       = true
  eq CredAcq        CredAcq        = true
  eq CredDisp       CredDisp       = true
  eq CredRefr       CredRefr       = true
  eq AnomPromisc    AnomPromisc    = true
  eq Login          Login          = true
  eq _              _              = false

instance eqEventTypeLinux :: Eq EventType where
  eq Success Success = true
  eq Failure Failure = true
  eq _       _       = false

instance eqEventLinux :: Eq Event where
  eq (Event x) (Event y) = (x == y)

eventCategories :: Array EventCategory
eventCategories =
  [ DaemonStart
  , ConfigChange
  , SystemBoot
  , SystemRunLevel
  , ServiceStart
  , NetfilterCfg
  , Syscall
  , Proctitle
  , ServiceStop
  , UserStart
  , UserCmd
  , UserEnd
  , UserLogin
  , UserAuth
  , UserAcct
  , CredAcq
  , CredDisp
  , CredRefr
  , AnomPromisc
  , Login
  ]

eventTypes :: Array EventType
eventTypes = [ Success, Failure ]

uri :: Event -> String
uri (Event x) = JSON.stringify $ unsafeCoerce $
  { eventCategory : show x.eventCategory
  , eventType     : show x.eventType
  , eventID       : show x.eventID
  , eventURI      : show x.eventURI
  , eventTime     : Event.foreignEventTime x.eventTime
  , eventSource   : Event.foreignEventSource x.eventSource
  }
