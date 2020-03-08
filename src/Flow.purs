module Flow
  ( Entry (..)
  , parse
  , unparse
  , insert
  , schema
  , createReader
  ) where

import Prelude

import Control.Alt ((<|>))

import Control.Coroutine (Producer)
import Control.Coroutine.Aff (produce, emit)

import Control.Monad.Trans.Class (lift)

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

import Effect.Exception (Error)
import Effect.Exception (error) as Exception

import Data.Array as Array
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Data.Either (Either(..))

import Data.Identity (Identity)
import Data.Newtype (unwrap)

import Data.Foldable (foldl)
import Data.Traversable(foldMap)
import Data.String.CodeUnits (singleton)

import Text.Parsing.Parser (Parser, fail, runParser)
import Text.Parsing.Parser.String (char, eof, satisfy, string)
import Text.Parsing.Parser.Combinators (choice)

import Date as Date
import HTTP as HTTP
import Socket as Socket
import Stream as Stream
import Process as Process
import Readline as Readline

import UUIDv1 as UUIDv1
import UUIDv5 as UUIDv5

import DB as DB

newtype Entry = Entry
  { sIP      :: String
  , dIP      :: String
  , sPort    :: String
  , dPort    :: String
  , protocol :: String
  , packets  :: String
  , bytes    :: String
  , flags    :: String
  , sTime    :: String
  , duration :: String
  , eTime    :: String
  , sensor   :: String
  }

instance showEntry :: Show Entry where
  show (Entry entry) = "(Entry " <> show entry <> ")"

delimiter :: Char
delimiter = ','

parseValue :: Parser String String
parseValue = foldMap singleton <$> List.many (satisfy $ not <<< eq delimiter)

{-- Parses a valid digit, 0-9, or fails otherwise. --}
digit :: Parser String String
digit = choice (string <$> ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"])

{-- Parses a valid octet, 0-255, or fails otherwise. --}
octet :: Parser String String
octet = do
  w <- Array.many digit
  case w of
    [x, y, z] -> pure (x <> y <> z)
    [x, y]    -> pure (x <> y)
    [x]       -> pure x
    _         -> fail "Invalid octet."

{-- Parses a valid IPv4 address, 0.0.0.0-255.255.255.255, or fails otherwise. --}
ipv4 :: Parser String String
ipv4 = do
  w <- octet
  _ <- string dot
  x <- octet
  _ <- string dot
  y <- octet
  _ <- string dot
  z <- octet
  pure (w <> dot <> x <> dot <> y <> dot <> z)
  where dot = "."

id :: forall a. a -> a
id x = x

{-- Parses a valid port number, 0-65535, or failw otherwise. --}
port :: Parser String String
port = do
  w <- foldMap id <$> List.many digit
  case Array.elemIndex w ports of
    (Just _)  -> pure w
    (Nothing) -> fail "Invalid port."
  where ports = show <$> Array.range 0 65535

{-- Parses a valid TCP flag. --}
flag :: Parser String String
flag = choice (string <$> ["U", "A", "P", "R", "S", "F"])

{-- Parses a valid string of TCP flags. --}
flags :: Parser String String
flags = do
  elems <- List.many flag
  count <- pure $ List.length elems
  case (count >= 0) && (count <= 6) of
    true  -> pure $ foldMap id elems
    false -> fail "Invalid number of TCP flags."

{-- Parses a valid string of digits. --}
digits :: Parser String String
digits = foldMap id <$> List.many digit

{-- Parses a valid duration for SiLk flow record. --}
duration :: Parser String String
duration = do
  w <- digit
  _ <- string dot
  x <- digit
  y <- digit
  z <- digit
  pure (w <> dot <> x <> y <> z)
  where dot = "." 

{-- Parses a valid sTime or eTime for a SiLk flow record (assume UTC time). --}
time :: Parser String String
time = do
  year   <- digits
  _      <- string "/"
  month  <- digits
  _      <- string "/"
  day    <- digits
  _      <- string "T"
  hour   <- digits
  _      <- string ":"
  minute <- digits
  _      <- string ":"
  second <- digits
  _      <- string "."
  millis <- digits
  case Date.isValid (format year month day hour minute second millis) of
    true  -> pure $ format' year month day hour minute second millis
    false -> fail "Invalid sTime or eTime."
  where
    format year month day hour minute second millis = foldl (<>) year $
      [ "-"
      , month
      , "-"
      , day
      , "T"
      , hour
      , ":"
      , minute
      , ":"
      , second
      , "."
      , millis
      , "Z"
      ]
    format' year month day hour minute second millis = foldl (<>) year $
      [ "/"
      , month
      , "/"
      , day
      , "T"
      , hour
      , ":"
      , minute
      , ":"
      , second
      , "."
      , millis
      ]

{-- Parses a valid lowercase letter, or fails otherwise. --}
lowercase :: Parser String String
lowercase = choice (string <$> letters)
  where 
    letters = ["a", "b", "c", "d", "e", "f", "g", "h"]
      <> ["i", "j", "k", "l", "m", "n", "o", "p"] 
      <> ["q", "r", "s", "t", "u", "v", "w", "x"]
      <> ["y", "z"]

{-- Parses a valid uppercase letter, or fails otherwise. --}
uppercase :: Parser String String
uppercase = choice (string <$> letters)
  where 
    letters = ["A", "B", "C", "D", "E", "F", "G", "H"]
      <> ["I", "J", "K", "L", "M", "N", "O", "P"] 
      <> ["Q", "R", "S", "T", "U", "V", "W", "Z"]
      <> ["Y", "Z"]

{-- Parses a valid name for a SiLk sensor from a set of whitelisted characters. --}
sensor :: Parser String String
sensor = foldMap id <$> List.many whitelist
  where  
    other     = choice (string <$> ["-", "_", "."])
    whitelist = lowercase <|> uppercase <|> digit <|> other

{-- Parses a valid SiLk flow record based on the parsers defined for its fields, or fails otherwise. --}
parse :: Parser String Entry
parse = do
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
  packets   <- digits
  _         <- comma
  bytes     <- digits
  _         <- comma
  flags'    <- flags
  _         <- comma
  sTime     <- time
  _         <- comma
  duration' <- duration
  _         <- comma
  eTime     <- time
  _         <- comma
  sensor'   <- sensor
  _         <- eof
  pure $ Entry
    { sIP      : sIP
    , dIP      : dIP
    , sPort    : sPort
    , dPort    : dPort
    , protocol : protocol
    , packets  : packets
    , bytes    : bytes
    , flags    : flags'
    , sTime    : sTime
    , duration : duration'
    , eTime    : eTime
    , sensor   : sensor'
    }
  where comma = char delimiter

insert :: DB.Database -> Entry -> HTTP.IncomingMessage -> DB.Request Unit
insert filename (Entry entry) req = do
  timestamp <- lift $ liftEffect (Date.toISOString <$> Date.current)
  DB.insert filename table $ params timestamp
  where
    params timestamp = 
      [ Tuple "LogID" logID
      , Tuple "SourceID" sourceID
      , Tuple "EntryID" entryID
      , Tuple "SIP" entry.sIP
      , Tuple "DIP" entry.dIP
      , Tuple "SPort" entry.sPort
      , Tuple "DPort" entry.dPort
      , Tuple "Protocol" entry.protocol
      , Tuple "Packets" entry.packets
      , Tuple "Bytes" entry.bytes
      , Tuple "Flags" entry.flags
      , Tuple "STime" entry.sTime
      , Tuple "Duration" entry.duration
      , Tuple "ETime" entry.eTime
      , Tuple "Sensor" entry.sensor
      ]
    remoteAddress = Socket.remoteAddress $ HTTP.socket req
    remotePort    = Socket.remotePort $ HTTP.socket req
    remotePort'   = show remotePort
    entryID       = UUIDv5.namespaceUUID sourceID $ HTTP.messageURL req
    sourceID      = UUIDv5.namespaceUUID UUIDv1.defaultUUID $ remoteAddress
    logID         = UUIDv1.defaultUUID

schema :: DB.Database -> DB.Request Unit
schema filename = DB.schema filename table params $
  [ Tuple "SIP" DB.Text
  , Tuple "DIP" DB.Text
  , Tuple "SPort" DB.Text
  , Tuple "DPort" DB.Text
  , Tuple "Protocol" DB.Text
  , Tuple "Packets" DB.Text
  , Tuple "Bytes" DB.Text
  , Tuple "Flags" DB.Text
  , Tuple "STime" DB.Text
  , Tuple "Duration" DB.Text
  , Tuple "ETime" DB.Text
  , Tuple "Sensor" DB.Text
  ]
  where params = [ Tuple "LogID" DB.Text, Tuple "SourceID" DB.Text, Tuple "EntryID" DB.Text ]

table :: DB.Table
table = "Flow"

unparse'' :: Entry -> String
unparse'' (Entry entry) = foldl (\x y -> x <> delimiter' <> y) entry.sIP $
  [ entry.dIP
  , entry.sPort
  , entry.dPort
  , entry.protocol
  , entry.packets
  , entry.bytes
  , entry.flags
  , entry.sTime
  , entry.duration
  , entry.eTime
  , entry.sensor
  ]
  where delimiter' = singleton delimiter

unparse' :: Entry -> Identity (Either Error String)
unparse' entry = do
  expect     <- pure $ unparse'' entry
  result'    <- pure $ flip runParser parse $ (unparse'' entry)
  case result' of
    (Left error) -> do
      let result'' = { error : error, expect : expect, entry : entry }
      pure $ Left (Exception.error $ show result'')
    (Right entry')    -> do
      check <- pure $ unparse'' entry'
      let result'' = { check  : check, expect : expect, entry : entry }
      case check == expect of
        true -> do
          pure $ Right expect
        false -> do
          pure $ Left (Exception.error $ show result'')

unparse :: Entry -> Either Error String
unparse entry = unwrap $ unparse' entry

createReader' :: Readline.Interface -> Producer Entry Aff Unit
createReader' interface = produce \emitter -> do
  Readline.onLine (\line -> emit' emitter $ line) $ interface
  where
    emit' emitter = \line -> do
      result <- pure $ flip runParser parse $ line
      case result of
        (Left error)  -> pure unit
        (Right entry) -> emit emitter $ entry

createReader :: Stream.Readable -> Effect (Producer Entry Aff Unit)
createReader readable = do
  interface <- Readline.createInterface readable Process.stdout false 
  pure $ createReader' interface
