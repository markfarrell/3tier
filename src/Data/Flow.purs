module Data.Flow
  ( Event (..)
  , Flag (..)
  , event
  ) where

import Prelude

import Data.List as List
import Data.Foldable (intercalate)

import Data.Traversable(foldMap)

import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.String (char, eof, string)
import Text.Parsing.Parser.Combinators (choice)

import Unsafe.Coerce (unsafeCoerce)

import FFI.Date (Date)
import FFI.JSON as JSON

import Parser (date, port, ipv4, octet, positiveInteger)
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

delimiter :: Char
delimiter = ','

flag :: Parser String Flag
flag = choice [urg, rst, fin, syn, psh, ack]
  where
    urg = do
      _ <- string "U"
      pure U
    rst = do
      _ <- string "R"
      pure R
    fin = do
      _ <- string "F"
      pure F
    syn = do
      _ <- string "S"
      pure S
    psh = do
      _ <- string "P"
      pure F
    ack = do
      _ <- string "A"
      pure A

flags :: Parser String (Array Flag)
flags = do
  elems <- List.many (pure <$> flag)
  count <- pure $ List.length elems
  case (count >= 0) && (count <= 6) of
    true  -> pure $ foldMap identity elems
    false -> fail "Invalid number of TCP flags."

{-- Parses a valid SiLk event event based on the parsers defined for its fields, or fails otherwise. --}
event :: Parser String Event
event = do
  sIP       <- ipv4
  _         <- comma
  dIP       <- ipv4
  _         <- comma
  sPort     <- port
  _         <- comma
  dPort     <- port
  _         <- comma
  protocol  <- octet
  _         <- comma
  packets   <- positiveInteger
  _         <- comma
  bytes     <- positiveInteger
  _         <- comma
  flags'    <- flags
  _         <- comma
  startTime <- date
  _         <- comma
  duration' <- positiveInteger
  _         <- comma
  endTime   <- date
  _         <- eof
  pure $ Event
    { sIP       : sIP
    , dIP       : dIP
    , sPort     : sPort
    , dPort     : dPort
    , protocol  : protocol
    , packets   : packets
    , bytes     : bytes
    , flags     : flags'
    , startTime : startTime
    , duration  : duration'
    , endTime   : endTime
    }
  where comma = char delimiter

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
