module Data.Windows.Security
  ( Event(..)
  , EventID
  , MachineName
  , CategoryName
  , CategoryNumber
  , Message(..)
  , Description
  , Source
  , EntryNumber
  , EntryData
  , EntryType
  , ReplacementStrings
  , InstanceID
  , Site
  , Container
  , SecurityID
  , AccountName
  , AccountDomain
  , LogonID
  ) where

import Prelude

import Foreign (Foreign)

import Unsafe.Coerce (unsafeCoerce)

import FFI.Date (Date)
import FFI.JSON as JSON

type EventID            = Int
type MachineName        = String
type CategoryName       = String
type CategoryNumber     = Int
type Description        = Array Message
type Source             = String
type EntryNumber        = Int
type EntryType          = String
type EntryData          = String
type ReplacementStrings = String
type InstanceID         = String
type Site               = String
type Container          = String

type SecurityID    = String
type AccountName   = String
type AccountDomain = String
type LogonID       = String

data Message = Subject
  { securityID    :: SecurityID
  , accountName   :: AccountName
  , accountDomain :: AccountDomain
  , logonID       :: LogonID
  }

data Event = Event
  { eventID            :: EventID
  , machineName        :: MachineName
  , entryNumber        :: EntryNumber 
  , entryData          :: EntryData
  , category           :: CategoryName
  , categoryNumber     :: CategoryNumber
  , entryType          :: EntryType
  , description        :: Description
  , source             :: Source
  , replacementStrings :: ReplacementStrings
  , instanceID         :: InstanceID
  , timeGenerated      :: Date
  , timeWritten        :: Date
  , site               :: Site
  , container          :: Container 
  }

foreignMessage :: Message -> Foreign
foreignMessage (Subject y) = unsafeCoerce $
    { securityID    : y.securityID
    , accountName   : y.accountName
    , accountDomain : y.accountDomain
    , logonID       : y.logonID
    }

foreignDescription :: Description -> Foreign
foreignDescription x = unsafeCoerce (foreignMessage <$> x)

instance showEventWindowsEvent :: Show Event where
  show (Event x) = JSON.stringify $ unsafeCoerce $
    { eventID            : show x.eventID
    , machineName        : x.machineName
    , entryNumber        : show x.entryNumber
    , entryData          : x.entryData
    , category           : x.category
    , categoryNumber     : show x.categoryNumber
    , entryType          : x.entryType
    , description        : foreignDescription x.description
    , source             : x.source 
    , replacementStrings : x.replacementStrings
    , instanceID         : x.instanceID
    , timeGenerated      : show x.timeGenerated
    , timeWritten        : show x.timeWritten
    , site               : x.site
    , container          : x.container
    }

instance eqMessageWindowsEvent :: Eq Message where
  eq (Subject x) (Subject y) = (x == y)

instance eqEventWindowsEvent :: Eq Event where
  eq (Event x) (Event y) = (x == y)
