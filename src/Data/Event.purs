module Data.Event
  ( Event(..)
  , Entity(..)
  , Time(..)
  ) where

import Prelude

import Unsafe.Coerce (unsafeCoerce)

import FFI.Date (Date)
import FFI.JSON (stringify) as JSON

import Data.IPv4 (IPv4)

data Time = Time
  { startTime :: Date
  , duration  :: Int
  , endTime   :: Date 
  }

data Entity = Host
  { ip   :: IPv4
  , port :: Int
  }

data Event a b c d = Event
  { eventCategory :: a
  , eventType     :: b
  , eventID       :: c
  , eventURI      :: d
  , eventTime     :: Time
  , eventSource   :: Entity
  } 

instance showTimeData :: Show Time where
  show (Time x) = JSON.stringify $ unsafeCoerce $
    { startTime : show x.startTime
    , duration  : show x.duration
    , endTime   : show x.endTime
    }

instance showEntityData :: Show Entity where
  show (Host x) = JSON.stringify $ unsafeCoerce $
    { ip   : show x.ip
    , port : show x.port
    }

instance eqEntityData :: Eq Entity where
  eq (Host x) (Host y) = (x == y)

instance eqTimeData :: Eq Time where
  eq (Time x) (Time y) = (x == y)
