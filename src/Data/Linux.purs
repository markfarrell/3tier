module Data.Linux
 ( Event
 , EventCategory(..)
 , EventID(..)
 ) where

import Prelude

import Foreign.Class (class Marshall) as F
import Foreign.Coerce (coerce) as F

import Data.Event as E
import Data.EventType (EventType)

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
  | DaemonEnd

data EventID = EventID Int

type Event = E.Event EventCategory EventType EventID

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
  show DaemonEnd      = "DAEMON-END"

instance showEventIDLinux :: Show EventID where
  show (EventID x) = show x

derive instance eqEventCategoryLinux :: Eq EventCategory

derive instance eqEventIDLinux :: Eq EventID

instance marshallEventCategory :: F.Marshall EventCategory where
  marshall = F.coerce <<< show

instance marshallEventID :: F.Marshall EventID where
  marshall = F.coerce <<< show

instance eventCategoryLinux :: E.EventCategory EventCategory where
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
    , DaemonEnd
    ]

instance eventIDLinux :: E.EventID EventID where
  eventIDs = []
