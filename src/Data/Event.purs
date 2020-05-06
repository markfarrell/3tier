module Data.Event
  ( Event(..)
  , Source(..)
  , Time(..)
  , foreignTime
  , foreignSource
  ) where

import Prelude

import Data.Foldable (foldl)

import Foreign (Foreign)

import Unsafe.Coerce (unsafeCoerce)

import FFI.Date (Date)
import FFI.JSON (stringify) as JSON

import Data.IPv4 (IPv4)

data Time = Time
  { startTime :: Date
  , duration  :: Int
  , endTime   :: Date 
  }

data Source = Tier1 | Tier2 | Tier3 | Host { ip :: IPv4, port :: Int }

data Event a b c d = Event
  { eventCategory :: a
  , eventType     :: b
  , eventID       :: c
  , eventURI      :: d
  , eventTime     :: Time
  , eventSource   :: Source
  } 

instance showTimeData :: Show Time where
  show (Time x) = JSON.stringify $ unsafeCoerce $
    { startTime : show x.startTime
    , duration  : show x.duration
    , endTime   : show x.endTime
    }

instance showSourceData :: Show Source where
  show (Tier1)  = "TIER-01"
  show (Tier2)  = "TIER-02"
  show (Tier3)  = "TIER-03"
  show (Host x) = show x.ip <> ":" <> show x.port

instance eqSourceData :: Eq Source where
  eq (Host x) (Host y) = (x == y)
  eq (Tier1)  (Tier1)  = true
  eq (Tier2)  (Tier2)  = true
  eq (Tier3)  (Tier3)  = true
  eq _        _        = false

instance eqTimeData :: Eq Time where
  eq (Time x) (Time y) = foldl (&&) true comparison
    where
      comparison =
        [ eq x.startTime y.startTime
        , eq x.duration y.duration
        , eq x.endTime y.endTime
        ]

foreignTime :: Time -> Foreign
foreignTime (Time x) = unsafeCoerce $
  { startTime : show x.startTime
  , duration  : show x.duration
  , endTime   : show x.endTime
  }

foreignSource :: Source -> Foreign
foreignSource source = unsafeCoerce $ show source
