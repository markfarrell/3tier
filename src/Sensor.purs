module Sensor
  ( Entry (..)
  , parseEntry
  , writeEntry
  , insert
  ) where

import Prelude

import Control.Monad.Trans.Class (lift)

import Effect (Effect)
import Effect.Class (liftEffect)

import Effect.Exception (Error)
import Effect.Exception (error) as Exception

import Data.Either (Either(..))

import Data.Identity (Identity)
import Data.Newtype (unwrap)

import Data.Foldable (foldl)
import Data.Traversable(foldMap)
import Data.String.CodeUnits (singleton)
import Data.List(many)

import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.String (char, satisfy)

import Date as Date
import HTTP as HTTP
import Socket as Socket
import Strings as Strings

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
parseValue = foldMap singleton <$> many (satisfy $ not <<< eq delimiter)

parseEntry :: Parser String Entry
parseEntry = do
  sIP      <- parseValue
  _        <- char delimiter
  dIP      <- parseValue
  _        <- char delimiter
  sPort    <- parseValue
  _        <- char delimiter
  dPort    <- parseValue
  _        <- char delimiter
  protocol <- parseValue
  _        <- char delimiter
  packets  <- parseValue
  _        <- char delimiter
  bytes    <- parseValue
  _        <- char delimiter
  flags    <- parseValue
  _        <- char delimiter
  sTime    <- parseValue
  _        <- char delimiter
  duration <- parseValue
  _        <- char delimiter
  eTime    <- parseValue
  _        <- char delimiter
  sensor   <- parseValue
  pure $ Entry
    { sIP      : sIP
    , dIP      : dIP
    , sPort    : sPort
    , dPort    : dPort
    , protocol : protocol
    , packets  : packets
    , bytes    : bytes
    , flags    : flags
    , sTime    : sTime
    , duration : duration
    , eTime    : eTime
    , sensor   : sensor
    }

insertQuery :: Entry -> HTTP.IncomingMessage -> Effect String
insertQuery (Entry entry) req = do
  timestamp <- Date.toISOString <$> Date.current
  pure $ query timestamp
  where
    query timestamp = "INSERT INTO Sensor (" <> columns <> ") VALUES ('" <> values timestamp <> "')"
    columns = foldl (\x y -> x <> "," <> y) "Timestamp" $
      [ "RemoteAddress"
      , "RemotePort"
      , "URL"
      , "SIP"
      , "DIP"
      , "SPort"
      , "DPort"
      , "Protocol"
      , "Packets"
      , "Bytes"
      , "Flags"
      , "STime"
      , "Duration"
      , "ETime"
      , "Sensor"
      ]
    values timestamp = foldl (\x y -> x <> "','" <> y) timestamp $
      [ remoteAddress
      , remotePort'
      , url'
      , entry.sIP
      , entry.dIP
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
    remoteAddress = Socket.remoteAddress $ HTTP.socket req
    remotePort = Socket.remotePort $ HTTP.socket req
    remotePort' = show remotePort
    url' = Strings.encodeBase64 $ HTTP.messageURL req

insert :: Entry -> HTTP.IncomingMessage -> DB.Request Unit
insert entry = \req -> do
  query <- lift $ liftEffect $ insertQuery entry req
  DB.insert filename query
  where filename = "logs.db"

writeEntry'' :: Entry -> String
writeEntry'' (Entry entry) = foldl (\x y -> x <> delimiter' <> y) entry.sIP $
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

writeEntry' :: Entry -> Identity (Either Error String)
writeEntry' entry = do
  expect     <- pure $ writeEntry'' entry
  result'    <- pure $ flip runParser parseEntry $ (writeEntry'' entry)
  case result' of
    (Left error) -> do
      let result'' = { error : error, expect : expect, entry : entry }
      pure $ Left (Exception.error $ show result'')
    (Right entry')    -> do
      check <- pure $ writeEntry'' entry'
      let result'' = { check  : check, expect : expect, entry : entry }
      case check == expect of
        true -> do
          pure $ Right expect
        false -> do
          pure $ Left (Exception.error $ show result'')

writeEntry :: Entry -> Either Error String
writeEntry entry = unwrap $ writeEntry' entry
