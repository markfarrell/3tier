module Text.Parsing.Flow
  ( event
  ) where

import Prelude

import Data.List as List

import Data.Foldable (intercalate)

import Data.Traversable (foldMap) as Traversable

import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.String (char, eof, string)
import Text.Parsing.Parser.Combinators (choice)

import Parser (date, port, ipv4, octet, positiveInteger)

import Data.Flow as Flow

delimiter :: Char
delimiter = ','

flag :: Parser String Flow.Flag
flag = choice [urg, rst, fin, syn, psh, ack]
  where
    urg = do
      _ <- string "U"
      pure Flow.U
    rst = do
      _ <- string "R"
      pure Flow.R
    fin = do
      _ <- string "F"
      pure Flow.F
    syn = do
      _ <- string "S"
      pure Flow.S
    psh = do
      _ <- string "P"
      pure Flow.F
    ack = do
      _ <- string "A"
      pure Flow.A

flags :: Parser String (Array Flow.Flag)
flags = do
  elems <- List.many (pure <$> flag)
  count <- pure $ List.length elems
  case (count >= 0) && (count <= 6) of
    true  -> pure $ Traversable.foldMap identity elems
    false -> fail "Invalid number of TCP flags."

{-- Parses a valid SiLk event event based on the parsers defined for its fields, or fails otherwise. --}
event :: Parser String Flow.Event
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
  pure $ Flow.Event
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

