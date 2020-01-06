module Linux
 ( Entry
 , Message
 , MessageType
 , parseEntry
 , insert
 , summary
 )  where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Trans.Class (lift)

import Effect.Class (liftEffect)

import Data.Array (fromFoldable) as Array
import Data.Foldable(foldl)
import Data.Traversable (sequence)

import Foreign (readString) as Foreign
import Foreign.Index ((!))

import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.String (char, string)

import Date as Date
import DB as DB
import HTTP as HTTP
import Socket as Socket
import Strings as Strings

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
  DB.insert filename $ query timestamp field
  where
    query timestamp field = "INSERT INTO Linux (" <> columns <> ") VALUES ('" <> values timestamp field <> "')"
    columns = foldl (\x y -> x <> "," <> y) "Timestamp" $
      [ "RemoteAddress"
      , "RemotePort"
      , "URL"
      , "MessageType"
      , "Message"
      , "FieldName"
      , "FieldValue"
      ]
    values timestamp field = foldl (\x y -> x <> "','" <> y) timestamp $ 
      [ remoteAddress
      , remotePort'
      , url'
      , messageType'
      , message'
      , Fields.fieldName field
      , Fields.fieldValue field
      ]
    remoteAddress = Socket.remoteAddress $ HTTP.socket req
    remotePort = Socket.remotePort $ HTTP.socket req
    remotePort' = show remotePort
    url' = Strings.encodeBase64 $ HTTP.messageURL req
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
