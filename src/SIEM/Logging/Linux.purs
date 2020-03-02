module SIEM.Logging.Linux
 ( Entry
 , Message
 , MessageType
 , parseEntry
 , insert
 , schema
 , writeEntry
 , createReader
 )  where

import Prelude

import Control.Coroutine (Producer)
import Control.Coroutine.Aff (produce, emit)

import Control.Alternative ((<|>))
import Control.Monad.Trans.Class (lift)

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

import Effect.Exception (Error)
import Effect.Exception (error) as Exception

import Data.Either (Either(..))
import Data.Tuple (Tuple(..))

import Data.Identity (Identity)
import Data.Newtype (unwrap)

import Data.Array (fromFoldable) as Array
import Data.Foldable(foldl)
import Data.Traversable (sequence)
import Data.String.CodeUnits (singleton)

import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.String (char, string)

import Date as Date
import DB as DB
import HTTP as HTTP
import Socket as Socket

import Readline as Readline
import Stream as Stream
import Strings as Strings

import Process as Process

import UUIDv3 as UUIDv3

import SIEM.Logging.Linux.Fields as Fields
import SIEM.Logging.Session as Session

type Field = Fields.Field

delimiter :: Char
delimiter = ' '

parseMessageType :: Parser String MessageType
parseMessageType = do
  _  <- string "type"
  _  <- string "="
  ty <- parseMessageType' 
  pure ty
  where 
    parseMessageType' = parse DaemonStart
      <|> parse ConfigChange
      <|> parse SystemBoot
      <|> parse SystemRunLevel
      <|> parse ServiceStart
      <|> parse NetfilterCfg
      <|> parse SyscallEntry
      <|> parse ProctitleEntry
      <|> parse ServiceStop
      <|> parse UserStart
      <|> parse UserCmd
      <|> parse UserEnd
      <|> parse UserLogin
      <|> parse UserAcct
      <|> parse UserAuth
      <|> parse CredAcq
      <|> parse CredDisp
      <|> parse CredRefr
      <|> parse AnomPromiscuous
      <|> parse Login
    parse ty = do
       _ <- string (messageType ty)
       pure ty

parseMessage :: Parser String Message
parseMessage = do
  _  <- string "msg"
  _  <- string "="
  v  <- Fields.parseValue
  pure (Message v)

data MessageType =
    DaemonStart
  | ConfigChange
  | SystemBoot
  | SystemRunLevel
  | ServiceStart
  | NetfilterCfg
  | SyscallEntry
  | ProctitleEntry
  | ServiceStop
  | UserStart
  | UserCmd
  | UserEnd
  | UserLogin
  | UserAuth
  | UserAcct
  | CredAcq
  | CredDisp
  | CredRefr
  | AnomPromiscuous
  | Login

newtype Message = Message String

data FieldEntry = MessageType Message Field

data Entry = Entry MessageType Message (Array Field)

instance showMessageType :: Show MessageType where
  show (DaemonStart) = "(DaemonStart)"
  show (ConfigChange) = "(ConfigChange)"
  show (SystemBoot)   = "(SystemBoot)"
  show (ServiceStart) = "(ServiceStart)"
  show (SystemRunLevel) = "(SystemRunLevel)"
  show (NetfilterCfg) = "(NetfilterCfg)"
  show (SyscallEntry) = "(SyscallEntry)"
  show (ProctitleEntry) = "(ProctitleEntry)"
  show (ServiceStop) = "(ServiceStop)"
  show (UserStart) = "(UserStart)"
  show (UserCmd) = "(UserCmd)"
  show (UserEnd) = "(UserEnd)"
  show (UserLogin) = "(UserLogin)"
  show (UserAuth) = "(UserAuth)"
  show (UserAcct) = "(UserAcct)"
  show (CredAcq) = "(CredAcq)"
  show (CredDisp) = "(CredDisp)"
  show (CredRefr) = "(CredRefr)"
  show (AnomPromiscuous) = "(AnomPromiscuous)"
  show (Login) = "(Login)"

instance showMessage :: Show Message where
  show (Message x) = "(Message " <> show x <> ")"

instance showEntry :: Show Entry where
  show (Entry ty msg fields) = "(Entry " <> show ty <> " " <> show msg <> " " <> show fields <> ")"

parseEntry :: Parser String Entry
parseEntry = do
  ty     <- parseMessageType
  _      <- char Fields.delimiter
  msg    <- parseMessage
  _      <- char Fields.delimiter
  fields <- Array.fromFoldable <$> Fields.parseFields
  pure $ Entry ty msg fields 

messageType :: MessageType -> String
messageType (DaemonStart) = "DAEMON_START"
messageType (ConfigChange) = "CONFIG_CHANGE"
messageType (SystemBoot) = "SYSTEM_BOOT"
messageType (SystemRunLevel) = "SYSTEM_RUNLEVEL"
messageType (ServiceStart) = "SERVICE_START"
messageType (NetfilterCfg) = "NETFILTER_CFG"
messageType (SyscallEntry) = "SYSCALL"
messageType (ProctitleEntry) = "PROCTITLE"
messageType (ServiceStop) = "SERVICE_STOP"
messageType (UserStart) = "USER_START"
messageType (UserCmd) = "USER_CMD"
messageType (UserEnd) = "USER_END"
messageType (UserAuth) = "USER_AUTH"
messageType (UserAcct) = "USER_ACCT"
messageType (UserLogin) = "USER_LOGIN"
messageType (CredAcq) = "CRED_ACQ"
messageType (CredDisp) = "CRED_DISP"
messageType (CredRefr) = "CRED_REFR"
messageType (AnomPromiscuous) = "ANOM_PROMISCUOUS"
messageType (Login) = "LOGIN"

message :: Message -> String
message (Message msg) = msg

insert' :: String -> Entry -> HTTP.IncomingMessage -> Array (DB.Request Unit)
insert' filename (Entry ty msg fields) req = flip (<$>) fields $ \field -> do
  timestamp <- lift $ liftEffect (Date.toISOString <$> Date.current)
  logID     <- lift $ lift (Session.getLogID req)
  DB.insert filename table $ params timestamp logID field
  where
    params timestamp logID field =
      [ Tuple "Timestamp" timestamp
      , Tuple "RemoteAddress" remoteAddress
      , Tuple "RemotePort" remotePort'
      , Tuple "LogID" logID
      , Tuple "EntryID" (entryID logID)
      , Tuple "MessageType" messageType'
      , Tuple "Message" message'
      , Tuple "FieldName" (Fields.fieldName field)
      , Tuple "FieldValue" (Strings.encodeBase64 $ Fields.fieldValue field)
      ]
    remoteAddress = Socket.remoteAddress $ HTTP.socket req
    remotePort = Socket.remotePort $ HTTP.socket req
    remotePort' = show remotePort
    entryID logID = UUIDv3.namespaceUUID logID $ HTTP.messageURL req
    messageType' = messageType ty
    message' = message msg
    table = "Linux"

insert :: String -> Entry -> HTTP.IncomingMessage -> DB.Request Unit
insert filename entry req = do
  _ <- sequence $ insert' filename entry req
  lift $ pure unit

schema :: String -> DB.Request Unit
schema filename = DB.schema filename "Linux" $
  [ Tuple "Timestamp" DB.TextNotNull
  , Tuple "RemoteAddress" DB.TextNotNull
  , Tuple "RemotePort" DB.TextNotNull
  , Tuple "LogID" DB.TextNotNull
  , Tuple "EntryID" DB.TextNotNull
  , Tuple "MessageType" DB.TextNotNull
  , Tuple "FieldName" DB.TextNotNull
  , Tuple "FieldValue" DB.TextNotNull
  ]

writeEntry'' :: Entry -> String
writeEntry'' (Entry ty msg fields) = foldl (\x y -> x <> delimiter' <> y) (ty') $ [msg'] <> fields'
  where 
    delimiter'   = singleton delimiter
    ty'          = (\t -> "type" <> "=" <> messageType t) $ ty
    msg'         = (\(Message v) -> "msg" <> "=" <> v) $ msg
    fields'      = (\field -> Fields.fieldName field <> "=" <> Fields.fieldValue field) <$> fields

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
        (Left error)  -> pure unit
        (Right entry) -> emit emitter $ entry

createReader :: Stream.Readable -> Effect (Producer Entry Aff Unit)
createReader readable = do
  interface <- Readline.createInterface readable Process.stdout false 
  pure $ createReader' interface
