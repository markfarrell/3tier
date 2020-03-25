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

import Report (Report)
import Report as Report

type Connection = SQLite3.Database

type Settings = String

type Table = String

data Schema  = Audit | Flow

data Insert  = InsertAudit Audit.Entry | InsertFlow Flow.Entry

type Select  = Report

data Query   = InsertQuery Insert | SelectQuery Select

data ResultSet = InsertResult Unit | SelectResult Report.Entry

instance showSchema :: Show Schema where
  show Audit = "Audit"
  show Flow  = "Flow"

data RequestDSL a = InsertRequest Insert HTTP.IncomingMessage (ResultSet -> a) | SelectRequest Select HTTP.IncomingMessage (ResultSet -> a)

instance functorRequestDSL :: Functor RequestDSL where
  map :: forall a b. (a -> b) -> RequestDSL a -> RequestDSL b
  map f (InsertRequest query' req next)  = (InsertRequest query' req (f <<< next))
  map f (SelectRequest query' req next)  = (SelectRequest query' req (f <<< next))

type Interpreter = WriterT (Array String) Aff 

type Request a = FreeT RequestDSL Interpreter a

type Result a = Either Error (Tuple a (Array String))

type Column = Tuple String ColumnType
 
data ColumnType = Text | Real

schemaURI' :: Table -> Array Column -> Array Column -> String
schemaURI' table' params params' = query
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

schemaURI :: Schema -> String
schemaURI Audit = schemaURI' "Audit" [] $
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
schemaURI Flow = schemaURI' "Flow" compositeKey $ 
  [ Tuple "SIP" Text
  , Tuple "DIP" Text
  , Tuple "SPort" Text
  , Tuple "DPort" Text
  , Tuple "Protocol" Text
  , Tuple "Packets" Text
  , Tuple "Bytes" Text
  , Tuple "Flags" Text
  , Tuple "STime" Text
  , Tuple "Duration" Real
  , Tuple "ETime" Text
  , Tuple "Sensor" Text
  ]
  where compositeKey = [ Tuple "LogID" Text, Tuple "SourceID" Text, Tuple "EntryID" Text ]

insertAuditURI :: Audit.Entry -> HTTP.IncomingMessage -> Aff String
insertAuditURI (Audit.Entry eventType eventID duration msg) req = do
  timestamp <- liftEffect $ (Date.toISOString <$> Date.current)
  pure $ insertURI' table (params timestamp)
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

insertFlowURI :: Flow.Entry -> HTTP.IncomingMessage -> Aff String
insertFlowURI (Flow.Entry entry) req = do
  timestamp <- liftEffect (Date.toISOString <$> Date.current)
  pure $ insertURI' table (params timestamp)
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
      , Tuple "Duration" (show entry.duration)
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

insertURI' ::  Table -> Array (Tuple String String) -> String
insertURI'  table' params = query
  where
     query = "INSERT INTO " <> table' <> " (" <> columns <> ") VALUES (" <> values <> ")"
     columns  = "'" <> (Arrays.join "','" columns') <> "'"
     values   = "'" <> (Arrays.join "','" values') <> "'"
     columns' = fst <$> params
     values'  = snd <$> params

insertURI :: Insert -> HTTP.IncomingMessage -> Aff String
insertURI (InsertAudit entry) req = insertAuditURI entry req
insertURI (InsertFlow  entry) req = insertFlowURI entry req


reportURI' :: Report.ReportType -> Table -> Table
reportURI' Report.Sources   = \table -> "SELECT COUNT(DISTINCT EntryID) AS X FROM (" <> table <> ") GROUP BY LogID, SourceID" 
reportURI' Report.Durations = \table -> "SELECT Duration as X FROM (" <> table <> ")"

reportURI :: Select -> Table
reportURI (Report.Audit eventID eventType reportType) = reportURI' reportType $ "SELECT * FROM Audit WHERE EventID='" <> show eventID <> "' AND EventType='" <> show eventType <> "'"

maxURI :: Select -> String
maxURI report = "SELECT MAX(X) AS Y FROM (" <> (reportURI report) <> ")"

minURI :: Select -> String
minURI report = "SELECT MIN(X) AS Y FROM (" <> (reportURI report) <> ")"

averageURI :: Select -> String
averageURI report = "SELECT AVG(X) AS Y FROM (" <> (reportURI report) <> ")"

sumURI :: Select -> String
sumURI report = "SELECT SUM(X) AS Y FROM (" <> (reportURI report) <> ")"

totalURI :: Select -> String
totalURI report = "SELECT COUNT(*) as Y FROM (" <> (reportURI report) <> ")"

varianceURI :: Select -> Number -> String
varianceURI report avg = query
  where 
    query  = "SELECT AVG(" <> query' <> " * " <> query' <> ") AS Y FROM (" <> (reportURI report) <> ")"
    query' = "(X - " <> show avg <> ")"

interpret :: forall a. Settings -> RequestDSL (Request a) -> Interpreter (Request a)
interpret settings (InsertRequest query' req next) = do
  _        <- tell ["INSERT"]
  _        <- lift $ executeTouch settings
  _        <- lift $ executeSchemas settings
  database <- lift $ SQLite3.connect settings SQLite3.OpenReadWrite
  uri      <- lift $ insertURI query' req
  _        <- lift $ SQLite3.all uri database
  _        <- lift $ SQLite3.close database
  lift (next <$> (pure $ InsertResult unit))
interpret settings (SelectRequest query' req next) = do
  _         <- tell ["SELECT"]
  _         <- lift $ executeTouch settings
  _         <- lift $ executeSchemas settings
  database  <- lift $ SQLite3.connect settings SQLite3.OpenReadOnly
  result    <- lift $ executeSelect database query'
  _         <- lift $ SQLite3.close database
  lift (next <$> (pure result))

executeTouch :: Settings -> Aff Unit
executeTouch settings = do
  database <- SQLite3.connect settings SQLite3.OpenCreate
  _        <- SQLite3.close database
  pure unit

executeSchemas :: Settings -> Aff Unit
executeSchemas settings = do
  database <- SQLite3.connect settings SQLite3.OpenReadWrite
  _        <- SQLite3.all (schemaURI Flow) database
  _        <- SQLite3.all (schemaURI Audit) database
  _        <- SQLite3.close database
  pure unit

executeSelect :: Connection -> Select -> Aff ResultSet
executeSelect database report = do
  min        <- executeSelect' database (minURI report)
  max        <- executeSelect' database (maxURI report)
  sum        <- executeSelect' database (sumURI report)
  total      <- executeSelect' database (totalURI report)
  average    <- executeSelect' database (averageURI report)
  variance   <- executeSelect' database (varianceURI report average)
  pure $ SelectResult $ Report.Entry $
    { min       : min
    , max       : max
    , sum       : sum
    , total     : total
    , average   : average
    , variance  : variance
    }

executeSelect' :: Connection -> String -> Aff Number
executeSelect' database uri = do
  rows    <- SQLite3.all uri database
  results <- sequence (runResult <$> rows)
  case results of
    [number'] -> pure number'
    _         -> throwError error
  where
    runResult row = do
       result' <- pure (runExcept $ row ! "Y" >>= Foreign.readNumber)
       case result' of
         (Left _)        -> pure 0.0
         (Right number') -> pure number'
    error = Exception.error "Unexpected results."

request :: Query -> HTTP.IncomingMessage -> Request ResultSet
request (InsertQuery query') req = liftFreeT $ (InsertRequest query' req identity)
request (SelectQuery query') req = liftFreeT $ (SelectRequest query' req identity)

execute ::  forall a. Settings -> Request a -> Aff (Result a)
execute settings request' = try $ runWriterT $ runFreeT (interpret settings) request'
