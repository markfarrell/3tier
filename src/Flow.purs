module Flow
  ( Entry (..)
  , flow
  , write
  ) where

import Prelude

import Control.Alt ((<|>))

import Effect.Exception (Error)
import Effect.Exception (error) as Exception

import Data.Array as Array
import Data.List as List
import Data.Maybe (Maybe(..))

import Data.Either (Either(..))

import Data.Identity (Identity)
import Data.Newtype (unwrap)

import Data.Foldable (foldl)
import Data.Traversable(foldMap)
import Data.String.CodeUnits (singleton)

import Text.Parsing.Parser (Parser, fail, runParser)
import Text.Parsing.Parser.String (char, eof, string)
import Text.Parsing.Parser.Combinators (choice)

import Parser as Parser

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
  , duration :: Number
  , eTime    :: String
  , sensor   :: String
  }

instance showEntry :: Show Entry where
  show (Entry entry) = "(Entry " <> show entry <> ")"

delimiter :: Char
delimiter = ','

{-- Parses a valid octet, 0-255, or fails otherwise. --}
octet :: Parser String String
octet = do
  w <- Array.many Parser.digit
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

{-- Parses a valid port number, 0-65535, or failw otherwise. --}
port :: Parser String String
port = do
  w <- foldMap identity <$> List.many Parser.digit
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
    true  -> pure $ foldMap identity elems
    false -> fail "Invalid number of TCP flags."

{-- Parses a valid name for a SiLk sensor from a set of whitelisted characters. --}
sensor :: Parser String String
sensor = foldMap identity <$> List.many whitelist
  where  
    other     = choice (string <$> ["-", "_", "."])
    whitelist = Parser.lowercase <|> Parser.uppercase <|> Parser.digit <|> other

{-- Parses a valid SiLk flow record based on the parsers defined for its fields, or fails otherwise. --}
flow :: Parser String Entry
flow = do
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
  packets   <- Parser.digits
  _         <- comma
  bytes     <- Parser.digits
  _         <- comma
  flags'    <- flags
  _         <- comma
  sTime     <- Parser.timestamp
  _         <- comma
  duration' <- Parser.positiveFloat
  _         <- comma
  eTime     <- Parser.timestamp
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

write'' :: Entry -> String
write'' (Entry entry) = foldl (\x y -> x <> delimiter' <> y) entry.sIP $
  [ entry.dIP
  , entry.sPort
  , entry.dPort
  , entry.protocol
  , entry.packets
  , entry.bytes
  , entry.flags
  , entry.sTime
  , show entry.duration
  , entry.eTime
  , entry.sensor
  ]
  where delimiter' = singleton delimiter

write' :: Entry -> Identity (Either Error String)
write' entry = do
  expect     <- pure $ write'' entry
  result'    <- pure $ flip runParser flow $ (write'' entry)
  case result' of
    (Left error) -> do
      let result'' = { error : error, expect : expect, entry : entry }
      pure $ Left (Exception.error $ show result'')
    (Right entry')    -> do
      check <- pure $ write'' entry'
      let result'' = { check  : check, expect : expect, entry : entry }
      case check == expect of
        true -> do
          pure $ Right expect
        false -> do
          pure $ Left (Exception.error $ show result'')

write :: Entry -> Either Error String
write entry = unwrap $ write' entry
