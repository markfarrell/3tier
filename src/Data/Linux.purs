module Data.Linux
 ( EventCategory(..)
 , EventType(..)
 , EventID
 , Event(..)
 , eventCategories
 ) where

import Prelude

import Data.IPv4 (IPv4)

import FFI.Date (Date)
import FFI.JSON as JSON

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

data Event = Event
  { eventCategory :: EventCategory
  , eventType     :: EventType
  , eventID       :: EventID
  , startTime     :: Date
  , duration      :: Int
  , endTime       :: Date
  , sIP           :: IPv4
  , sPort         :: Int
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

uri :: Event -> String
uri (Event event') = JSON.stringify $ unsafeCoerce $
  { eventCategory : show event'.eventCategory
  , eventType     : show event'.eventType
  , eventID       : event'.eventID
  , startTime     : show event'.startTime
  , duration      : event'.duration
  , endTime       : show event'.endTime
  , sIP           : show event'.sIP
  , sPort         : event'.sPort
  }
