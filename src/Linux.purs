module Linux
 ( Field
 , Entry
 , Message
 , MessageType
 , parseEntry 
 , entryQueries
 )  where

import Prelude

import Control.Alternative ((<|>))
import Effect (Effect)
import Effect.Console (log)

import Data.Array (fromFoldable) as Array
import Data.Traversable(foldMap)
import Data.List(List, many)
import Data.String.CodeUnits (singleton)

import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Combinators(sepBy)
import Text.Parsing.Parser.String (string, satisfy)

foreign import escapeString :: String -> String

data Field =
    A0 String
  | A1 String
  | A2 String
  | A3 String
  | Acct String
  | Addr String
  | Arch String
  | AuditBacklogLimit String
  | AuditFailure String
  | Auid String
  | Cmd String
  | Comm String
  | Dev String
  | Egid String
  | Entries String
  | Euid String
  | Exe String
  | Exit String
  | Family String
  | Format String
  | Fsgid String
  | Fsuid String
  | Gid String
  | Grantors String
  | Hostname String
  | Items String
  | Kernel String
  | Key String
  | NewLevel String
  | Old String
  | OldAuid String
  | OldSes String
  | OldProm String
  | Op String
  | Pid String
  | Ppid String
  | Proctitle String
  | Prom String
  | Res String
  | Ses String
  | Sgid String
  | Success String
  | Suid String
  | Syscall String
  | Table String
  | Terminal String
  | Tty String
  | Uid String
  | Ver String
  | Msg String

instance showField :: Show Field where
  show (A0 v) = "(A0 " <> show v <> ")"
  show (A1 v) = "(A1 " <> show v <> ")"
  show (A2 v) = "(A2 " <> show v <> ")"
  show (A3 v) = "(A3 " <> show v <> ")"
  show (Acct v) = "(Acct " <> show v <> ")"
  show (Addr v) = "(Addr " <> show v <> ")"
  show (Arch v) = "(Arch " <> show v <> ")"
  show (AuditBacklogLimit v) = "(AuditBacklogLimit " <> show v <> ")"
  show (AuditFailure v) = "(AuditFailure " <> show v <> ")"
  show (Auid v) = "(Auid " <> show v <> ")"
  show (Cmd v) = "(Cmd " <> show v <> ")"
  show (Comm v) = "(Comm " <> show v <> ")"
  show (Dev v) = "(Dev " <> show v <> ")"
  show (Egid v) = "(Egid " <> show v <> ")"
  show (Entries v) = "(Entries " <> show v <> ")"
  show (Euid v) = "(Euid " <> show v <> ")"
  show (Exe v) = "(Exe " <> show v <> ")"
  show (Exit v) = "(Exit " <> show v <> ")"
  show (Family v) = "(Family " <> show v <> ")"
  show (Format v) = "(Format " <> show v <> ")"
  show (Fsgid v) = "(Fsgid " <> show v <> ")"
  show (Fsuid v) = "(Fsuid " <> show v <> ")"
  show (Gid v) = "(Gid " <> show v <> ")"
  show (Grantors v) = "(Grantors " <> show v <> ")"
  show (Hostname v) = "(Hostname " <> show v <> ")"
  show (Items v) = "(Items " <> show v <> ")"
  show (Kernel v) = "(Kernel " <> show v <> ")"
  show (Key v) = "(Key " <> show v <> ")"
  show (NewLevel v) = "(NewLevel " <> show v <> ")"
  show (Old v) = "(Old " <> show v <> ")"
  show (OldAuid v) = "(OldAuid " <> show v <> ")"
  show (OldSes v) = "(OldSes " <> show v <> ")"
  show (OldProm v) = "(OldProm " <> show v <> ")"
  show (Op v) = "(Op " <> show v <> ")"
  show (Pid v) = "(Pid " <> show v <> ")"
  show (Ppid v) = "(Ppid " <> show v <> ")"
  show (Proctitle v) = "(Proctitle " <> show v <> ")"
  show (Prom v) = "(Prom " <> show v <> ")"
  show (Res v) = "(Res " <> show v <> ")"
  show (Ses v) = "(Ses " <> show v <> ")"
  show (Sgid v) = "(Sgid " <> show v <> ")"
  show (Success v) = "(Success " <> show v <> ")"
  show (Suid v) = "(Suid " <> show v <> ")"
  show (Syscall v) = "(Syscall " <> show v <> ")"
  show (Table v) = "(Table " <> show v <> ")"
  show (Terminal v) = "(Terminal " <> show v <> ")"
  show (Tty v) = "(Tty " <> show v <> ")"
  show (Uid v) = "(Uid " <> show v <> ")"
  show (Ver v) = "(Ver " <> show v <> ")"
  show (Msg v) = "(Msg " <> show v <> ")"


parseMsg :: Parser String Field
parseMsg = do
  _ <- string "msg"
  _ <- string "="
  v <- parseValue
  pure (A0 v)

parseA0 :: Parser String Field
parseA0 = do
  _ <- string "a0"
  _ <- string "="
  v <- parseValue
  pure (A0 v)

parseA1 :: Parser String Field
parseA1 = do
  _ <- string "a1"
  _ <- string "="
  v <- parseValue
  pure (A1 v)

parseA2 :: Parser String Field
parseA2 = do
  _ <- string "a2"
  _ <- string "="
  v <- parseValue
  pure (A2 v)

parseA3 :: Parser String Field
parseA3 = do
  _ <- string "a3"
  _ <- string "="
  v <- parseValue
  pure (A3 v)

parseAcct :: Parser String Field
parseAcct = do
  _ <- string "acct"
  _ <- string "="
  v <- parseValue
  pure (Acct v)

parseAddr :: Parser String Field
parseAddr = do
  _ <- string "addr"
  _ <- string "="
  v <- parseValue
  pure (Addr v)

parseArch :: Parser String Field
parseArch = do
  _ <- string "arch"
  _ <- string "="
  v <- parseValue
  pure (Arch v)

parseAuditBacklogLimit :: Parser String Field
parseAuditBacklogLimit = do
  _ <- string "audit_backlog_limit"
  _ <- string "="
  v <- parseValue
  pure (AuditBacklogLimit v)

parseAuditFailure :: Parser String Field
parseAuditFailure = do
  _ <- string "audit_failure"
  _ <- string "="
  v <- parseValue
  pure (AuditFailure v)

parseAuid :: Parser String Field
parseAuid = do
  _ <- string "auid"
  _ <- string "="
  v <- parseValue
  pure (Auid v)

parseCmd :: Parser String Field
parseCmd = do
  _ <- string "cmd"
  _ <- string "="
  v <- parseValue
  pure (Cmd v)

parseComm :: Parser String Field
parseComm = do
  _ <- string "comm"
  _ <- string "="
  v <- parseValue
  pure (Comm v)

parseDev :: Parser String Field
parseDev = do
  _ <- string "dev"
  _ <- string "="
  v <- parseValue
  pure (Dev v)

parseEgid :: Parser String Field
parseEgid = do
  _ <- string "egid"
  _ <- string "="
  v <- parseValue
  pure (Egid v)

parseEntries :: Parser String Field
parseEntries = do
  _ <- string "entries"
  _ <- string "="
  v <- parseValue
  pure (Entries v)

parseEuid :: Parser String Field
parseEuid = do
  _ <- string "euid"
  _ <- string "="
  v <- parseValue
  pure (Euid v)

parseExe :: Parser String Field
parseExe = do
  _ <- string "exe"
  _ <- string "="
  v <- parseValue
  pure (Exe v)

parseExit :: Parser String Field
parseExit = do
  _ <- string "exit"
  _ <- string "="
  v <- parseValue
  pure (Exit v)

parseFamily :: Parser String Field
parseFamily = do
  _ <- string "family"
  _ <- string "="
  v <- parseValue
  pure (Family v)

parseFormat :: Parser String Field
parseFormat = do
  _ <- string "format"
  _ <- string "="
  v <- parseValue
  pure (Format v)

parseFsgid :: Parser String Field
parseFsgid = do
  _ <- string "fsgid"
  _ <- string "="
  v <- parseValue
  pure (Fsgid v)

parseFsuid :: Parser String Field
parseFsuid = do
  _ <- string "fsuid"
  _ <- string "="
  v <- parseValue
  pure (Fsuid v)

parseGid :: Parser String Field
parseGid = do
  _ <- string "gid"
  _ <- string "="
  v <- parseValue
  pure (Gid v)

parseGrantors :: Parser String Field
parseGrantors = do
  _ <- string "grantors"
  _ <- string "="
  v <- parseValue
  pure (Grantors v)

parseHostname :: Parser String Field
parseHostname = do
  _ <- string "hostname"
  _ <- string "="
  v <- parseValue
  pure (Hostname v)

parseItems :: Parser String Field
parseItems = do
  _ <- string "items"
  _ <- string "="
  v <- parseValue
  pure (Items v)

parseKernel :: Parser String Field
parseKernel = do
  _ <- string "kernel"
  _ <- string "="
  v <- parseValue
  pure (Kernel v)

parseKey :: Parser String Field
parseKey = do
  _ <- string "key"
  _ <- string "="
  v <- parseValue
  pure (Key v)

parseNewLevel :: Parser String Field
parseNewLevel = do
  _ <- string "new-level"
  _ <- string "="
  v <- parseValue
  pure (NewLevel v)

parseOld :: Parser String Field
parseOld = do
  _ <- string "old"
  _ <- string "="
  v <- parseValue
  pure (Old v)

parseOldAuid :: Parser String Field
parseOldAuid = do
  _ <- string "old-auid"
  _ <- string "="
  v <- parseValue
  pure (OldAuid v)

parseOldSes :: Parser String Field
parseOldSes = do
  _ <- string "old-ses"
  _ <- string "="
  v <- parseValue
  pure (OldSes v)

parseOldProm :: Parser String Field
parseOldProm = do
  _ <- string "old_prom"
  _ <- string "="
  v <- parseValue
  pure (OldProm v)

parseOp :: Parser String Field
parseOp = do
  _ <- string "op"
  _ <- string "="
  v <- parseValue
  pure (Op v)

parsePid :: Parser String Field
parsePid = do
  _ <- string "pid"
  _ <- string "="
  v <- parseValue
  pure (Pid v)

parsePpid :: Parser String Field
parsePpid = do
  _ <- string "ppid"
  _ <- string "="
  v <- parseValue
  pure (Ppid v)

parseProctitle :: Parser String Field
parseProctitle = do
  _ <- string "proctitle"
  _ <- string "="
  v <- parseValue
  pure (Proctitle v)

parseProm :: Parser String Field
parseProm = do
  _ <- string "prom"
  _ <- string "="
  v <- parseValue
  pure (Prom v)

parseRes :: Parser String Field
parseRes = do
  _ <- string "res"
  _ <- string "="
  v <- parseValue
  pure (Res v)

parseSes :: Parser String Field
parseSes = do
  _ <- string "ses"
  _ <- string "="
  v <- parseValue
  pure (Ses v)

parseSgid :: Parser String Field
parseSgid = do
  _ <- string "sgid"
  _ <- string "="
  v <- parseValue
  pure (Sgid v)

parseSuccess :: Parser String Field
parseSuccess = do
  _ <- string "success"
  _ <- string "="
  v <- parseValue
  pure (Success v)

parseSuid :: Parser String Field
parseSuid = do
  _ <- string "suid"
  _ <- string "="
  v <- parseValue
  pure (Suid v)

parseSyscall :: Parser String Field
parseSyscall = do
  _ <- string "syscall"
  _ <- string "="
  v <- parseValue
  pure (Syscall v)

parseTable :: Parser String Field
parseTable = do
  _ <- string "table"
  _ <- string "="
  v <- parseValue
  pure (Table v)

parseTerminal :: Parser String Field
parseTerminal = do
  _ <- string "terminal"
  _ <- string "="
  v <- parseValue
  pure (Terminal v)

parseTty :: Parser String Field
parseTty = do
  _ <- string "tty"
  _ <- string "="
  v <- parseValue
  pure (Tty v)

parseUid :: Parser String Field
parseUid = do
  _ <- string "uid"
  _ <- string "="
  v <- parseValue
  pure (Uid v)

parseVer :: Parser String Field
parseVer = do
  _ <- string "ver"
  _ <- string "="
  v <- parseValue
  pure (Ver v)

parseField :: Parser String Field
parseField = parseA0
  <|> parseA1
  <|> parseA2
  <|> parseA3
  <|> parseAcct
  <|> parseAddr
  <|> parseArch
  <|> parseAuditBacklogLimit
  <|> parseAuditFailure
  <|> parseAuid
  <|> parseCmd
  <|> parseComm
  <|> parseDev
  <|> parseEgid
  <|> parseEntries
  <|> parseEuid
  <|> parseExe
  <|> parseExit
  <|> parseFamily
  <|> parseFormat
  <|> parseFsgid
  <|> parseFsuid
  <|> parseGid
  <|> parseGrantors
  <|> parseHostname
  <|> parseItems
  <|> parseKernel
  <|> parseKey
  <|> parseNewLevel
  <|> parseOldAuid
  <|> parseOldSes
  <|> parseOldProm
  <|> parseOld
  <|> parseOp
  <|> parsePid
  <|> parsePpid
  <|> parseProctitle
  <|> parseProm
  <|> parseRes
  <|> parseSes
  <|> parseSgid
  <|> parseSuccess
  <|> parseSuid
  <|> parseSyscall
  <|> parseTable
  <|> parseTerminal
  <|> parseTty
  <|> parseUid
  <|> parseVer
  <|> parseMsg

fieldName :: Field -> String
fieldName (A0 _) = "a0"
fieldName (A1 _) = "a1"
fieldName (A2 _) = "a2"
fieldName (A3 _) = "a3"
fieldName (Acct _) = "acct"
fieldName (Addr _) = "addr"
fieldName (Arch _) = "arch"
fieldName (AuditBacklogLimit _) = "audit_backlog_limit"
fieldName (AuditFailure _) = "audit_failure"
fieldName (Auid _) = "auid"
fieldName (Cmd _) = "cmd"
fieldName (Comm _) = "comm"
fieldName (Dev _) = "dev"
fieldName (Egid _) = "egid"
fieldName (Entries _) = "entries"
fieldName (Euid _) = "euid"
fieldName (Exe _) = "exe"
fieldName (Exit _) = "exit"
fieldName (Family _) = "family"
fieldName (Format _) = "format"
fieldName (Fsgid _) = "fsgid"
fieldName (Fsuid _) = "fsuid"
fieldName (Gid _) = "gid"
fieldName (Grantors _) = "grantors"
fieldName (Hostname _) = "hostname"
fieldName (Items _) = "items"
fieldName (Kernel _) = "kernel"
fieldName (Key _) = "key"
fieldName (NewLevel _) = "new-level"
fieldName (Old _) = "old"
fieldName (OldAuid _) = "old-auid"
fieldName (OldSes _) = "old-ses"
fieldName (OldProm _) = "old_prom"
fieldName (Op _) = "op"
fieldName (Pid _) = "pid"
fieldName (Ppid _) = "ppid"
fieldName (Proctitle _) = "proctitle"
fieldName (Prom _) = "prom"
fieldName (Res _) = "res"
fieldName (Ses _) = "ses"
fieldName (Sgid _) = "sgid"
fieldName (Success _) = "success"
fieldName (Suid _) = "suid"
fieldName (Syscall _) = "syscall"
fieldName (Table _) = "table"
fieldName (Terminal _) = "terminal"
fieldName (Tty _) = "tty"
fieldName (Uid _) = "uid"
fieldName (Ver _) = "ver"
fieldName (Msg _) = "msg"

fieldValue :: Field -> String
fieldValue (A0 v) = v
fieldValue (A1 v) = v
fieldValue (A2 v) = v
fieldValue (A3 v) = v
fieldValue (Acct v) = v
fieldValue (Addr v) = v
fieldValue (Arch v) = v
fieldValue (AuditBacklogLimit v) = v
fieldValue (AuditFailure v) = v
fieldValue (Auid v) = v
fieldValue (Cmd v) = v
fieldValue (Comm v) = v
fieldValue (Dev v) = v
fieldValue (Egid v) = v
fieldValue (Entries v) = v
fieldValue (Euid v) = v
fieldValue (Exe v) = v
fieldValue (Exit v) = v
fieldValue (Family v) = v
fieldValue (Format v) = v
fieldValue (Fsgid v) = v
fieldValue (Fsuid v) = v
fieldValue (Gid v) = v
fieldValue (Grantors v) = v
fieldValue (Hostname v) = v
fieldValue (Items v) = v
fieldValue (Kernel v) = v
fieldValue (Key v) = v
fieldValue (NewLevel v) = v
fieldValue (Old v) = v
fieldValue (OldAuid v) = v
fieldValue (OldSes v) = v
fieldValue (OldProm v) = v
fieldValue (Op v) = v
fieldValue (Pid v) = v
fieldValue (Ppid v) = v
fieldValue (Proctitle v) = v
fieldValue (Prom v) = v
fieldValue (Res v) = v
fieldValue (Ses v) = v
fieldValue (Sgid v) = v
fieldValue (Success v) = v
fieldValue (Suid v) = v
fieldValue (Syscall v) = v
fieldValue (Table v) = v
fieldValue (Terminal v) = v
fieldValue (Tty v) = v
fieldValue (Uid v) = v
fieldValue (Ver v) = v
fieldValue (Msg v) = v

parseValue :: Parser String String
parseValue = foldMap singleton <$> many (satisfy $ (\c -> not ((eq '+' c) || (eq '\n' c))))

parseFields :: Parser String (List Field)
parseFields = parseField `sepBy` string "+"

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
  v  <- parseValue
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
  _      <- string "+"
  msg    <- parseMessage
  _      <- string "+"
  fields <- Array.fromFoldable <$> parseFields
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

fieldQuery :: String -> String -> MessageType -> Message -> Field -> String
fieldQuery uuid sourceHost ty msg field = "INSERT INTO Linux (UUID, SourceHost, MessageType, Message, FieldName, FieldValue) VALUES (" <> values <> ")"
  where values = "'" <> uuid <> "','" <> sourceHost <> "','" <> (messageType ty) <> "','" <> (message msg) <> "','" <> (fieldName field) <> "','" <> (escapeString <<< fieldValue $ field) <> "'" 

entryQueries :: String -> String -> Entry -> Array String
entryQueries uuid sourceHost (Entry ty msg fields) = fieldQuery' <$> fields
  where fieldQuery' = fieldQuery uuid sourceHost ty msg

test :: Effect Unit
test = do
  check <- pure $ runParser entry parseEntry
  log $ show $ (show check) == expect
  where 
    entry="type=DAEMON_START msg=audit(1575912248.984:3695): op=start ver=2.8.5 format=raw kernel=3.10.0-1062.1.2.el7.x86_64 auid=4294967295 pid=705 uid=0 ses=4294967295 res=success"
    expect="(Right (Entry (DaemonStart) (Message \"audit(1575912248.984:3695):\") [(Op \"start\"),(Ver \"2.8.5\"),(Format \"raw\"),(Kernel \"3.10.0-1062.1.2.el7.x86_64\"),(Auid \"4294967295\"),(Pid \"705\"),(Uid \"0\"),(Ses \"4294967295\"),(Res \"success\")]))"

