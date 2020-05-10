module Data.Linux
 ( Event
 , EventCategory(..)
 , EventID
 , eventCategories
 ) where

import Prelude

import Data.Event as Event

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

type EventID = Int

type Event = Event.Event EventCategory EventID

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

derive instance eqEventCategoryLinux :: Eq EventCategory

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
