module SIEM.Logging.Sensor
  ( Entry (..)
  , parseEntry
  , writeEntry
  , insert
  , createReader
  ) where

import Prelude

import Control.Coroutine (Producer)
import Control.Coroutine.Aff (produce, emit)

import Control.Monad.Trans.Class (lift)

import Control.Monad.Except (runExcept)

import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)

import Effect.Exception (Error)
import Effect.Exception (error, throw) as Exception

import Data.Either (Either(..))
import Data.Tuple (Tuple(..))

import Data.Identity (Identity)
import Data.Newtype (unwrap)

import Data.Foldable (foldl)
import Data.Traversable(foldMap)
import Data.String.CodeUnits (singleton)
import Data.List(many)

import Foreign (readString) as Foreign
import Foreign.Index ((!))

import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.String (char, satisfy)

import Date as Date
import HTTP as HTTP
import Socket as Socket
import Stream as Stream
import Process as Process
import Readline as Readline

import UUIDv3 as UUIDv3

import Audit as Audit
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

insert :: String -> Entry -> HTTP.IncomingMessage -> DB.Request Unit
insert filename (Entry entry) req = do
  timestamp <- lift $ liftEffect (Date.toISOString <$> Date.current)
  logID     <- lift $ liftEffect (createLogID)
  DB.insert filename table $ params timestamp logID
  where
    params timestamp logID = 
      [ Tuple "Timestamp" timestamp
      , Tuple "RemoteAddress" remoteAddress
      , Tuple "RemotePort" remotePort'
      , Tuple "LogID" logID
      , Tuple "EntryID" (entryID logID)
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
    remotePort = Socket.remotePort $ HTTP.socket req
    remotePort' = show remotePort
    entryID logID = UUIDv3.namespaceUUID logID $ HTTP.messageURL req
    table = "Sensor"
    createLogID' headers = runExcept $ do
      result <- headers ! "log-id" >>= Foreign.readString
      pure result
    createLogID = do
       result <- pure (createLogID' $ HTTP.messageHeaders req)
       case result of
         (Left _)      -> Exception.throw "Invalid request headers (Log-ID)."
         (Right logID) -> pure $ logID

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

createReader' :: Readline.Interface -> Producer Entry Aff Unit
createReader' interface = produce \emitter -> do
  Readline.onLine (\line -> emit' emitter $ line) $ interface
  where
    emit' emitter = \line -> do
      result <- pure $ flip runParser parseEntry $ line
      case result of
        (Left error)  -> do
           let result' = { line : line, error : error }
           _ <- debug $ Audit.Entry Audit.Failure Audit.DeserializationRequest (show result') 
           pure unit
        (Right entry) -> do
           let result' = { line : line, entry : entry }
           _ <- debug $ Audit.Entry Audit.Success Audit.DeserializationRequest (show result') 
           emit emitter $ entry
    debug entry = void $ launchAff $ Audit.debug entry

createReader :: Stream.Readable -> Effect (Producer Entry Aff Unit)
createReader readable = do
  interface <- Readline.createInterface readable Process.stdout false 
  pure $ createReader' interface
