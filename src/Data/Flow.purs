module Data.Flow
  ( Event (..)
  , Flag (..)
  ) where

import Prelude

import Data.Foldable (intercalate)

import Unsafe.Coerce (unsafeCoerce)

import FFI.Date (Date)
import FFI.JSON as JSON

import Data.IPv4 (IPv4)

data Flag = U | R | F | S | P | A

data Event = Event
  { sIP       :: IPv4
  , dIP       :: IPv4
  , sPort     :: Int
  , dPort     :: Int
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

instance showFlag :: Show Flag where
  show U = "U"
  show R = "R"
  show F = "F"
  show S = "S"
  show P = "P"
  show A = "A"

uri :: Event -> String
uri (Event event') = JSON.stringify $ unsafeCoerce $
  { sIP       : show event'.sIP
  , dIP       : show event'.dIP
  , sPort     : event'.sPort
  , dPort     : event'.dPort
  , protocol  : event'.protocol
  , packets   : event'.packets
  , bytes     : event'.bytes
  , flags     : intercalate "" (show <$> event'.flags)
  , startTime : show event'.startTime
  , duration  : event'.duration
  , end       : event'.endTime
  }
