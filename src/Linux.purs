module Linux
 ( Entry
 , Message
 , MessageType
 , parseEntry
 , insert
 , summary
 , writeEntry
 , createReader
 )  where

import Prelude

import Control.Coroutine (Producer)
import Control.Coroutine.Aff (produce, emit)

import Control.Alternative ((<|>))
import Control.Monad.Trans.Class (lift)

import Control.Monad.Except (runExcept)

import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)

import Effect.Exception (Error)
import Effect.Exception (error, throw) as Exception

import Data.Either (Either(..))

import Data.Identity (Identity)
import Data.Newtype (unwrap)

import Data.Array (fromFoldable) as Array
import Data.Foldable(foldl)
import Data.Traversable (sequence)
import Data.String.CodeUnits (singleton)

import Foreign (readString) as Foreign
import Foreign.Index ((!))

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

import Audit as Audit

import Linux.Fields as Fields

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

insert' :: Entry -> HTTP.IncomingMessage -> Array (DB.Request Unit)
insert' (Entry ty msg fields) req = flip (<$>) fields $ \field -> do
  timestamp <- lift $ liftEffect (Date.toISOString <$> Date.current)
  logID     <- lift $ liftEffect (createLogID)
  DB.insert filename $ query timestamp logID field
  where
    query timestamp logID field = "INSERT INTO Linux (" <> columns <> ") VALUES ('" <> values timestamp logID field <> "')"
    columns = foldl (\x y -> x <> "," <> y) "Timestamp" $
      [ "RemoteAddress"
      , "RemotePort"
      , "LogID"
      , "EntryID"
      , "MessageType"
      , "Message"
      , "FieldName"
      , "FieldValue"
      ]
    values timestamp logID field = foldl (\x y -> x <> "','" <> y) timestamp $ 
      [ remoteAddress
      , remotePort'
      , logID
      , entryID logID
      , messageType'
      , message'
      , Fields.fieldName field
      , Strings.encodeBase64 $ Fields.fieldValue field
      ]
    remoteAddress = Socket.remoteAddress $ HTTP.socket req
    remotePort = Socket.remotePort $ HTTP.socket req
    remotePort' = show remotePort
    entryID logID = UUIDv3.namespaceUUID logID $ HTTP.messageURL req
    createLogID' headers = runExcept $ do
      result <- headers ! "log-id" >>= Foreign.readString
      pure result
    createLogID = do
       result <- pure (createLogID' $ HTTP.messageHeaders req)
       case result of
         (Left _)      -> Exception.throw "Invalid request headers (Log-ID)."
         (Right logID) -> pure $ logID 
    messageType' = messageType ty
    message' = message msg
    filename = "logs.db"

insert :: Entry -> HTTP.IncomingMessage -> DB.Request Unit
insert entry req = do
  _ <- sequence $ insert' entry req
  lift $ pure unit

summary :: DB.Request (Array (Array String))
summary = DB.select filename query readResult
  where
    readResult row = do
       ty          <- row ! "MessageType" >>= Foreign.readString
       entries     <- row ! "Entries"     >>= Foreign.readString
       pure $ [ty, entries]
    query = "SELECT RemoteAddress as 'RemoteAddress', MessageType AS 'MessageType', CAST(COUNT(DISTINCT URL) AS TEXT) AS 'Entries' FROM Linux GROUP BY RemoteAddress, MessageType ORDER BY Entries DESC"
    filename = "logs.db"

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
