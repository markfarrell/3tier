module Tier3
 ( Request
 , Result
 , RequestDSL
 , Interpreter
 , Setting(..)
 , Settings(..)
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

import Data.Foldable (oneOf) as Foldable

import Data.Tuple (Tuple(..), fst, snd)

import Effect.Aff (Aff)
import Effect.Aff (parallel, sequential) as Aff
import Effect.Class (liftEffect)
import Effect.Exception (Error)
import Effect.Exception as Exception

import Foreign (readNumber) as Foreign
import Foreign.Index ((!))

import Arrays as Arrays

import Date as Date
import SQLite3 as SQLite3

import Audit as Audit
import Flow as Flow

import Forward as Forward

import Report as Report

import Route (Route)
import Route as Route

type Connection = SQLite3.Database

data Setting = Local String

data Settings = Settings (Array Setting)

type Table = String

type Column = Tuple String ColumnType
 
data ColumnType = Text | Real

data Schema  = AuditSchema | FlowSchema

type Insert = Forward.Forward

type Select = Report.Report

data ResultSet = InsertResult Unit | SelectResult Report.Entry

data RequestDSL a b = InsertRequest Settings Insert (a -> b) | SelectRequest Settings Select (a -> b)

type Interpreter = WriterT Audit.EventID Aff 

type Request a = FreeT (RequestDSL ResultSet) Interpreter a

type Result a = Either Error (Tuple a Audit.EventID)

instance functorRequestDSL :: Functor (RequestDSL a) where
  map :: forall b c. (b -> c) -> RequestDSL a b -> RequestDSL a c
  map f (InsertRequest settings query' next)  = (InsertRequest settings query' (f <<< next))
  map f (SelectRequest settings query' next)  = (SelectRequest settings query' (f <<< next))

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
schemaURI AuditSchema = schemaURI' "Audit" [] $
  [ Tuple "Timestamp" Text
  , Tuple "SourceIP" Text
  , Tuple "SourcePort" Text
  , Tuple "Duration" Real
  , Tuple "EventType" Text
  , Tuple "EventCategory" Text
  , Tuple "EventID" Text
  , Tuple "EventSource" Text
  ]
schemaURI FlowSchema = schemaURI' "Flow" [] $ 
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

insertAuditURI :: Audit.Entry -> Aff String
insertAuditURI (Audit.Entry entry) = do
  timestamp <- liftEffect $ (Date.toISOString <$> Date.current)
  pure $ insertURI' "Audit" (params timestamp)
  where 
    params timestamp =
      [ Tuple "Timestamp" timestamp 
      , Tuple "SourceIP" entry.sourceIP
      , Tuple "SourcePort" (show entry.sourcePort)
      , Tuple "Duration" (show entry.duration)
      , Tuple "EventType" (show entry.eventType)
      , Tuple "EventCategory" (show entry.eventCategory)
      , Tuple "EventID" (show entry.eventID)
      , Tuple "EventSource" (show entry.eventSource)
      ]

insertFlowURI :: Flow.Entry -> Aff String
insertFlowURI (Flow.Entry entry) = do
  pure $ insertURI' "Flow" params
  where
    params = 
      [ Tuple "SIP" entry.sIP
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

insertURI' ::  Table -> Array (Tuple String String) -> String
insertURI'  table' params = query
  where
     query = "INSERT INTO " <> table' <> " (" <> columns <> ") VALUES (" <> values <> ")"
     columns  = "'" <> (Arrays.join "','" columns') <> "'"
     values   = "'" <> (Arrays.join "','" values') <> "'"
     columns' = fst <$> params
     values'  = snd <$> params

insertURI :: Insert ->  Aff String
insertURI (Forward.Audit entry) = insertAuditURI entry
insertURI (Forward.Flow  entry) = insertFlowURI entry

reportAuditURI' :: Audit.ReportType -> Table -> Table
reportAuditURI' Audit.Sources   = \table -> "SELECT COUNT(*) AS X FROM (" <> table <> ") GROUP BY SourceIP, SourcePort" 
reportAuditURI' Audit.Durations = \table -> "SELECT Duration as X FROM (" <> table <> ")"

reportURI :: Select -> Table
reportURI (Report.Audit eventCategory eventType reportType) = reportAuditURI' reportType $ "SELECT * FROM Audit WHERE EventCategory='" <> show eventCategory <> "' AND EventType='" <> show eventType <> "'"

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

interpret :: forall a. RequestDSL ResultSet (Request a) -> Interpreter (Request a)
interpret (InsertRequest settings query' next) = do
  _      <- tell $ Route.eventID (Route.Forward query')
  result <- lift $ executeInsert settings query'
  lift (next <$> (pure result))
interpret (SelectRequest settings query' next) = do
  _      <- tell $ Route.eventID (Route.Report query')
  result <- lift $ executeSelect settings query'
  lift (next <$> (pure result))

executeTouch :: Setting -> Aff Unit
executeTouch (Local setting) = do
  database <- SQLite3.connect setting SQLite3.OpenCreate
  _        <- SQLite3.close database
  pure unit

executeSchemas :: Setting -> Aff Unit
executeSchemas (Local setting) = do
  database <- SQLite3.connect setting SQLite3.OpenReadWrite
  _        <- SQLite3.all (schemaURI FlowSchema) database
  _        <- SQLite3.all (schemaURI AuditSchema) database
  _        <- SQLite3.close database
  pure unit

executeInsert :: Settings -> Insert -> Aff ResultSet
executeInsert (Settings settings) query' = do
  result <- Aff.sequential (Foldable.oneOf (Aff.parallel <$> flip executeInsert' query' <$> settings)) 
  pure result

executeInsert' :: Setting -> Insert -> Aff ResultSet
executeInsert' setting query' = do
  _      <- executeTouch setting
  _      <- executeSchemas setting
  result <- executeInsert'' setting query'
  pure result

executeInsert'' :: Setting -> Insert -> Aff ResultSet
executeInsert'' (Local setting) query' = do
  database <- SQLite3.connect setting SQLite3.OpenReadWrite
  uri      <- insertURI query'
  _        <- SQLite3.all uri database
  _        <- SQLite3.close database
  pure (InsertResult unit)

executeSelect :: Settings -> Select -> Aff ResultSet
executeSelect (Settings settings) query' = do
  result <- Aff.sequential (Foldable.oneOf (Aff.parallel <$> flip executeSelect' query' <$> settings)) 
  pure result

executeSelect' :: Setting -> Select -> Aff ResultSet
executeSelect' setting query' = do
  _         <- executeTouch setting
  _         <- executeSchemas setting
  result    <- executeSelect'' setting query'
  pure result

executeSelect'' :: Setting -> Select -> Aff ResultSet
executeSelect'' (Local setting) report = do
  database   <- SQLite3.connect setting SQLite3.OpenReadOnly
  min        <- executeSelect''' database (minURI report)
  max        <- executeSelect''' database (maxURI report)
  sum        <- executeSelect''' database (sumURI report)
  total      <- executeSelect''' database (totalURI report)
  average    <- executeSelect''' database (averageURI report)
  variance   <- executeSelect''' database (varianceURI report average)
  _          <- SQLite3.close database
  pure $ SelectResult $ Report.Entry $
    { min       : min
    , max       : max
    , sum       : sum
    , total     : total
    , average   : average
    , variance  : variance
    }

executeSelect''' :: Connection -> String -> Aff Number
executeSelect''' database uri = do
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

request :: Settings -> Route -> Request ResultSet
request settings (Route.Forward query) = liftFreeT $ (InsertRequest settings query identity)
request settings (Route.Report query)  = liftFreeT $ (SelectRequest settings query identity)

execute ::  forall a. Request a -> Aff (Result a)
execute request' = try $ runWriterT $ runFreeT interpret request'
