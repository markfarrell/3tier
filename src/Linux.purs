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

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)

import Data.Array (fromFoldable) as Array
import Data.Foldable(foldl)

import Foreign (readString) as Foreign
import Foreign.Index ((!))

import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.String (char, string)

import Date as Date
import DB as DB
import HTTP as HTTP
import Socket as Socket
import Strings as Strings

import Linux.Fields as Fields

type Field = Fields.Field

delimiter :: Char
delimiter = '+'

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

insert :: Entry -> HTTP.IncomingMessage -> DB.Request Unit
insert (Entry ty msg _) req = do
  timestamp <- lift $ liftEffect (Date.toISOString <$> Date.current)
  DB.insert filename $ query timestamp
  where
    query timestamp  = "INSERT INTO Linux (Timestamp, RemoteAddress, RemotePort, URL, MessageType, Message) VALUES ('" <> values timestamp <> "')"
    values timestamp = foldl (\x y -> x <> "','" <> y) timestamp $ [remoteAddress, remotePort', url', messageType', message']
    remoteAddress = Socket.remoteAddress $ HTTP.socket req
    remotePort = Socket.remotePort $ HTTP.socket req
    remotePort' = show remotePort
    url' = Strings.encodeBase64 $ HTTP.messageURL req
    messageType' = messageType ty
    message' = message msg
    filename = "logs.db"

summary :: DB.Request (Array (Array String))
summary = DB.select filename query readResult
  where
    readResult row = do
       ty          <- row ! "MessageType" >>= Foreign.readString
       entries     <- row ! "Entries"     >>= Foreign.readString
       pure $ [ty, entries]
    query = "SELECT MessageType AS 'MessageType', CAST(COUNT(DISTINCT URL) AS TEXT) AS 'Entries' FROM Linux GROUP BY MessageType ORDER BY Entries DESC"
    filename = "logs.db"

test :: Effect Unit
test = do
  check <- pure $ runParser entry parseEntry
  log $ show $ (show check) == expect
  where 
    entry="type=DAEMON_START msg=audit(1575912248.984:3695): op=start ver=2.8.5 format=raw kernel=3.10.0-1062.1.2.el7.x86_64 auid=4294967295 pid=705 uid=0 ses=4294967295 res=success"
    expect="(Right (Entry (DaemonStart) (Message \"audit(1575912248.984:3695):\") [(Op \"start\"),(Ver \"2.8.5\"),(Format \"raw\"),(Kernel \"3.10.0-1062.1.2.el7.x86_64\"),(Auid \"4294967295\"),(Pid \"705\"),(Uid \"0\"),(Ses \"4294967295\"),(Res \"success\")]))"
