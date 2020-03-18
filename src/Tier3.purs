module Tier3
 ( RequestDSL
 , Interpreter
 , Request
 , Result
 , Settings
 , Table
 , ColumnType(..)
 , Schema(..)
 , Insert(..)
 , Select(..)
 , Query(..)
 , ResultSet(..)
 , request
 , execute
 , close
 , connect
 , insert
 , select
 , schema
 , touch
 , remove
 ) where

import Prelude

import Control.Monad.Except (runExcept, throwError)
import Control.Monad.Free.Trans (FreeT, liftFreeT, runFreeT)
import Control.Monad.Error.Class (try)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Class (tell)
import Control.Monad.Writer.Trans (WriterT, runWriterT)

import Data.Array as Array
import Data.Either (Either(..))
import Data.Traversable (sequence)

import Data.Tuple (Tuple(..), fst, snd)

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Exception as Exception

import Foreign (readNumber) as Foreign
import Foreign.Index ((!))

import Arrays as Arrays

import Date as Date
import HTTP as HTTP
import Socket as Socket
import SQLite3 as SQLite3
import UUIDv1 as UUIDv1
import UUIDv5 as UUIDv5

import Audit as Audit
import Flow as Flow

import Report as Report

type Settings = String

type Table = String

data Schema  = Audit | Flow

data Insert  = InsertAudit Audit.Entry | InsertFlow Flow.Entry

type Select  = Report.Report

data Query   = InsertQuery Insert | SelectQuery Select

data ResultSet = InsertResult Unit | SelectResult Report.Entry

instance showSchema :: Show Schema where
  show Audit = "Audit"
  show Flow  = "Flow"

data RequestDSL a = Close SQLite3.Database (Unit -> a)
  | Connect Settings SQLite3.Mode (SQLite3.Database -> a) 
  | Execute String SQLite3.Database (Array SQLite3.Row -> a)

instance functorRequestDSL :: Functor RequestDSL where
  map :: forall a b. (a -> b) -> RequestDSL a -> RequestDSL b
  map f (Close database next)        = (Close database (f <<< next))
  map f (Connect filename mode next) = (Connect filename mode (f <<< next))
  map f (Execute query database next)  = (Execute query database (f <<< next))

type Interpreter = WriterT (Array String) Aff 

type Request a = FreeT RequestDSL Interpreter a

type Result a = Either Error (Tuple a (Array String))

close :: SQLite3.Database -> Request Unit
close database = liftFreeT $ (Close database identity)

connect :: Settings -> SQLite3.Mode -> Request SQLite3.Database
connect filename mode = liftFreeT $ (Connect filename mode identity)

all :: String -> SQLite3.Database -> Request (Array SQLite3.Row)
all query database = liftFreeT $ (Execute query database identity)

insert' :: Settings -> Table -> Array (Tuple String String) -> Request Unit
insert' filename table' params = do
  database <- connect filename SQLite3.OpenReadWrite
  _        <- all query $ database
  _        <- close database
  lift $ pure unit
  where
     query = "INSERT INTO " <> table' <> " (" <> columns <> ") VALUES (" <> values <> ")"
     columns  = "'" <> (Arrays.join "','" columns') <> "'"
     values   = "'" <> (Arrays.join "','" values') <> "'"
     columns' = fst <$> params
     values'  = snd <$> params

select' :: forall a. (SQLite3.Row -> Aff a) -> Settings -> String -> Request (Array a)
select' runResult filename query = do
  database <- connect filename SQLite3.OpenReadOnly
  rows     <- all query $ database
  _        <- close database
  results  <- lift (lift $ sequence (runResult <$> rows))
  lift $ pure results

type Column = Tuple String ColumnType
 
data ColumnType = Text | Real

remove :: Settings -> Table -> Request Unit
remove filename table' = do
  database <- connect filename SQLite3.OpenReadWrite
  _        <- all query $ database
  _        <- close database
  lift $ pure unit
  where query = "DROP TABLE IF EXISTS " <> table'

schema' :: Settings -> Table -> Array Column -> Array Column -> Request Unit
schema' filename table' params params' = do
  database <- connect filename SQLite3.OpenReadWrite
  _        <- all query $ database
  _        <- close database
  lift $ pure unit
  where
     query   = case Array.length params > 0 of
       true  -> "CREATE TABLE IF NOT EXISTS " <> table' <> " (" <> columns <> "," <> primaryKey <> ")"
       false -> "CREATE TABLE IF NOT EXISTS " <> table' <> " (" <> columns <> ")"
     columns                = (Arrays.join "," columns')
     columns'               = column <$> (params <> params')
     column param           = Arrays.join " " $ [fst param, columnType $ snd param]
     columnType Text = "TEXT NOT NULL"
     columnType Real = "REAL NOT NULL"
     primaryKey       = "PRIMARY KEY (" <> primaryKey' <> ")"
     primaryKey'      = Arrays.join "," (fst <$> params)

schema :: Schema -> Settings -> Request Unit
schema Audit = \filename -> schema' filename "Audit" [] $
  [ Tuple "LogID" Text
  , Tuple "SourceID" Text
  , Tuple "EntryID" Text
  , Tuple "Timestamp" Text
  , Tuple "SourceAddress" Text
  , Tuple "SourcePort" Text
  , Tuple "Duration" Real
  , Tuple "EventType" Text
  , Tuple "EventID" Text
  , Tuple "Event" Text
  ]
schema Flow = \filename -> schema' filename "Flow" compositeKey $ 
  [ Tuple "SIP" Text
  , Tuple "DIP" Text
  , Tuple "SPort" Text
  , Tuple "DPort" Text
  , Tuple "Protocol" Text
  , Tuple "Packets" Text
  , Tuple "Bytes" Text
  , Tuple "Flags" Text
  , Tuple "STime" Text
  , Tuple "Duration" Text
  , Tuple "ETime" Text
  , Tuple "Sensor" Text
  ]
  where compositeKey = [ Tuple "LogID" Text, Tuple "SourceID" Text, Tuple "EntryID" Text ]

insertAudit :: Settings -> Audit.Entry -> HTTP.IncomingMessage -> Request ResultSet
insertAudit filename (Audit.Entry eventType eventID duration msg) req = do
  timestamp <- lift $ liftEffect $ (Date.toISOString <$> Date.current)
  result    <- insert' filename table $ params timestamp
  pure $ InsertResult result
  where 
    params timestamp =
      [ Tuple "LogID" logID
      , Tuple "SourceID" sourceID
      , Tuple "EntryID" entryID
      , Tuple "Timestamp" timestamp 
      , Tuple "SourceAddress" remoteAddress
      , Tuple "SourcePort" remotePort'
      , Tuple "Duration" duration'
      , Tuple "EventType" eventType'
      , Tuple "EventID" eventID'
      , Tuple "Event" msg
      ]
    remoteAddress = Socket.remoteAddress $ HTTP.socket req
    remotePort    = Socket.remotePort $ HTTP.socket req
    remotePort'   = show remotePort
    sourceID      = UUIDv5.namespaceUUID logID $ remoteAddress
    entryID       = UUIDv5.namespaceUUID sourceID $ HTTP.messageURL req
    logID         = UUIDv1.defaultUUID
    eventType'    = show eventType
    eventID'      = show eventID
    duration'     = show duration
    table         = "Audit"

insertFlow :: Settings -> Flow.Entry -> HTTP.IncomingMessage -> Request ResultSet
insertFlow filename (Flow.Entry entry) req = do
  timestamp <- lift $ liftEffect (Date.toISOString <$> Date.current)
  result    <- insert' filename table $ params timestamp
  pure $ InsertResult result
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
    table         = "Flow"

insert :: Settings -> Insert -> HTTP.IncomingMessage -> Request ResultSet
insert filename (InsertAudit entry) req = insertAudit filename entry req
insert filename (InsertFlow  entry) req = insertFlow filename entry req

touch :: Settings -> Request Unit
touch filename = do
  database <- connect filename SQLite3.OpenCreate
  _        <- close database
  lift $ pure unit

interpret :: forall a. RequestDSL (Request a) -> Interpreter (Request a)
interpret (Close database next) = do 
  _      <- tell ["CLOSE"]
  result <- lift $ next <$> SQLite3.close database
  lift $ pure result
interpret (Connect filename mode next) = do
  _      <- tell ["CONNECT"]
  result <- lift $ next <$> SQLite3.connect filename mode
  lift $ pure result 
interpret (Execute query database next) = do
  _      <- tell ["EXECUTE"]
  result <- lift $ next <$> SQLite3.all query database
  lift $ pure result
 

sample' :: Report.ReportType -> Table -> Table
sample' Report.Events    = \table -> "SELECT COUNT(DISTINCT EntryID) AS X FROM (" <> table <> ") GROUP BY LogID, SourceID" 
sample' Report.Durations = \table -> "SELECT Duration as X FROM (" <> table <> ")"

sample :: Select -> Table
sample (Report.Audit eventID eventType reportType) = sample' reportType $ "SELECT * FROM Audit WHERE EventID='" <> show eventID <> "' AND EventType='" <> show eventType <> "'"

sum :: Settings -> Select -> Request Number
sum filename report = do
  results <- select' runResult filename query
  case results of
    [sum']   -> pure sum'
    _        -> lift $ lift (throwError error)
  where
    runResult row = do
      result' <- pure (runExcept $ row ! "Result" >>= Foreign.readNumber)
      case result' of
        (Left _)     -> pure 0.0
        (Right sum') -> pure sum'
    error = Exception.error "Unexpected results."
    query = "SELECT SUM(X) AS Result FROM (" <> (sample report) <> ")"

average :: Settings -> Select ->  Request Number
average filename report = do
  results <- select' runResult filename query
  case results of
    []         -> pure 0.0
    [average'] -> pure average'
    _          -> lift $ lift (throwError error)
  where
    runResult row = do
      result' <- pure (runExcept $ row ! "Average" >>= Foreign.readNumber)
      case result' of
        (Left _)         -> pure 0.0
        (Right average') -> pure average'
    error = Exception.error "Unexpected results."
    query = "SELECT AVG(X) AS Average FROM (" <> (sample report) <> ")"

minimum :: Settings -> Select -> Request Number
minimum filename report = do
  results <- select' runResult filename query
  case results of
    [min'] -> pure min'
    _      -> lift $ lift (throwError error)
  where
    runResult row = do
      result' <- pure (runExcept $ row ! "Minimum" >>= Foreign.readNumber)
      case result' of
        (Left _)         -> pure 0.0
        (Right min')     -> pure min'
    error = Exception.error "Unexpected results."
    query = "SELECT MIN(X) AS Minimum FROM (" <> (sample report) <> ")"

maximum :: Settings -> Select -> Request Number
maximum filename report = do
  results <- select' runResult filename query
  case results of
    [max'] -> pure max'
    _      -> lift $ lift (throwError error)
  where
    runResult row = do
      result' <- pure (runExcept $ row ! "Maximum" >>= Foreign.readNumber)
      case result' of
        (Left _)         -> pure 0.0
        (Right max')     -> pure max'
    error = Exception.error "Unexpected results."
    query = "SELECT MAX(X) AS Maximum FROM (" <> (sample report) <> ")"

total :: Settings -> Select -> Request Number
total filename report = do
  results <- select' runResult filename query
  case results of
    [total'] -> pure total'
    _        -> lift $ lift (throwError error)
  where
    runResult row = do
       result' <- pure (runExcept $ row ! "Total" >>= Foreign.readNumber)
       case result' of
         (Left _)          -> pure 0.0
         (Right total')    -> pure total'
    error = Exception.error "Unexpected results."
    query = "SELECT COUNT(*) as Total FROM (" <> (sample report) <> ")"

variance' :: Settings -> Select -> Number -> Request Number
variance' filename report = \average' -> do
  results <- select' runResult filename $ query average'
  case results of
    [variance''] -> pure variance''
    _            -> lift $ lift (throwError error)
  where
    runResult row = do
       result' <- pure (runExcept $ row ! "Variance" >>= Foreign.readNumber)
       case result' of
         (Left _)          -> pure 0.0
         (Right variance'')    -> pure variance''
    error = Exception.error "Unexpected results."
    query average'  = "SELECT AVG(" <> query' average' <> " * " <> query' average' <> ") AS Variance FROM (" <> (sample report) <> ")"
    query' average' = "(X - " <> show average' <> ")"

select :: Settings -> Select -> HTTP.IncomingMessage -> Request ResultSet
select filename report _ = do
  min'       <- minimum filename report
  max'       <- maximum filename report
  sum'       <- sum filename report
  total'     <- total filename report
  average'   <- average filename report
  variance'' <- variance' filename report $ average'
  pure $ SelectResult $ Report.Entry $
    { min       : min'
    , max       : max'
    , sum       : sum'
    , total     : total'
    , average   : average'
    , variance  : variance''
    }

request :: Settings -> Query -> HTTP.IncomingMessage -> Request ResultSet
request filename (InsertQuery query') req = insert filename query' req
request filename (SelectQuery query') req = select filename query' req

execute ::  forall a. Request a -> Aff (Result a)
execute request' = try $ runWriterT $ runFreeT interpret request'
