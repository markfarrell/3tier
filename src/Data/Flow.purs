module Data.Flow
  ( Event (..)
  ) where

import Prelude

import Data.Foldable (intercalate)

import Unsafe.Coerce (unsafeCoerce)

import FFI.Date (Date)
import FFI.JSON as JSON

import Data.IPv4 (IPv4)
import Data.Port (Port)
import Data.TCP.Flag (Flag)

data Event = Event
  { sIP       :: IPv4
  , dIP       :: IPv4
  , sPort     :: Port
  , dPort     :: Port
  , protocol  :: Int
  , packets   :: Int
  , bytes     :: Int
  , flags     :: Array Flag
  , startTime :: Date
  , duration  :: Int
  , endTime   :: Date
  }

instance showEventFlow :: Show Event where
  show = uri

instance eqEventFlow :: Eq Event where
  eq (Event x) (Event y) = (x == y)

uri :: Event -> String
uri (Event event') = JSON.stringify $ unsafeCoerce $
  { sIP       : show event'.sIP
  , dIP       : show event'.dIP
  , sPort     : show event'.sPort
  , dPort     : show event'.dPort
  , protocol  : show event'.protocol
  , packets   : show event'.packets
  , bytes     : show event'.bytes
  , flags     : intercalate "" (show <$> event'.flags) 
  , startTime : show event'.startTime
  , duration  : show event'.duration
  , endTime   : show event'.endTime
  }
