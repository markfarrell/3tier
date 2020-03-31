module Tier3
 ( DSL
 , Request
 , Interpreter
 , Result
 , Setting(..)
 , Settings(..)
 , ResultSet(..)
 , request
 , execute
 ) where

import Prelude

import Control.Monad.Except (runExcept, throwError)
import Control.Monad.Free.Trans (liftFreeT, runFreeT)
import Control.Monad.Error.Class (try)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Class (tell)
import Control.Monad.Writer.Trans (runWriterT)

import Data.Array as Array
import Data.Either (Either(..))
import Data.Traversable (sequence)

import Data.Foldable (oneOf) as Foldable

import Data.Tuple (Tuple(..), fst, snd)

import Effect.Aff (Aff)
import Effect.Aff (parallel, sequential) as Aff
import Effect.Class (liftEffect)
import Effect.Exception as Exception

import Foreign (readNumber) as Foreign
import Foreign.Index ((!))

import Arrays as Arrays

import Date as Date
import SQLite3 as SQLite3

import Audit as Audit
import Flow as Flow

import Forward (Forward)
import Forward as Forward

import Report (Report)
import Report as Report

import Route (Route)
import Route as Route

import DSL as DSL

type Connection = SQLite3.Database

data Setting = Local String

data Settings = Settings (Array Setting)

type Table = String

type Column = Tuple String ColumnType
 
data ColumnType = Text | Real

data Schema  = AuditSchema | FlowSchema

data ResultSet = Forward Unit | Report Report.Entry

type DSL a = DSL.DSL Settings ResultSet a

type Request a = DSL.Request Settings ResultSet a

type Interpreter a = DSL.Interpreter a

type Result a = DSL.Result a

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

insertURI :: Forward ->  Aff String
insertURI (Forward.Audit entry) = insertAuditURI entry
insertURI (Forward.Flow  entry) = insertFlowURI entry

reportAuditURI' :: Audit.ReportType -> Table -> Table
reportAuditURI' Audit.Sources   = \table -> "SELECT COUNT(*) AS X FROM (" <> table <> ") GROUP BY SourceIP, SourcePort" 
reportAuditURI' Audit.Durations = \table -> "SELECT Duration as X FROM (" <> table <> ")"

reportURI :: Report -> Table
reportURI (Report.Audit eventCategory eventType reportType) = reportAuditURI' reportType $ "SELECT * FROM Audit WHERE EventCategory='" <> show eventCategory <> "' AND EventType='" <> show eventType <> "'"

maxURI :: Report -> String
maxURI report = "SELECT MAX(X) AS Y FROM (" <> (reportURI report) <> ")"

minURI :: Report -> String
minURI report = "SELECT MIN(X) AS Y FROM (" <> (reportURI report) <> ")"

averageURI :: Report -> String
averageURI report = "SELECT AVG(X) AS Y FROM (" <> (reportURI report) <> ")"

sumURI :: Report -> String
sumURI report = "SELECT SUM(X) AS Y FROM (" <> (reportURI report) <> ")"

totalURI :: Report -> String
totalURI report = "SELECT COUNT(*) as Y FROM (" <> (reportURI report) <> ")"

varianceURI :: Report -> Number -> String
varianceURI report avg = query
  where 
    query  = "SELECT AVG(" <> query' <> " * " <> query' <> ") AS Y FROM (" <> (reportURI report) <> ")"
    query' = "(X - " <> show avg <> ")"

interpret :: forall a. DSL (Request a) -> Interpreter (Request a)
interpret (DSL.Forward settings query' next) = do
  _      <- tell $ Route.eventID (Route.Forward query')
  result <- lift $ executeForward settings query'
  lift (next <$> (pure result))
interpret (DSL.Report settings query' next) = do
  _      <- tell $ Route.eventID (Route.Report query')
  result <- lift $ executeReport settings query'
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

executeForward :: Settings -> Forward -> Aff ResultSet
executeForward (Settings settings) query' = do
  result <- Aff.sequential (Foldable.oneOf (Aff.parallel <$> flip executeForward' query' <$> settings)) 
  pure result

executeForward' :: Setting -> Forward -> Aff ResultSet
executeForward' setting query' = do
  _      <- executeTouch setting
  _      <- executeSchemas setting
  result <- executeForward'' setting query'
  pure result

executeForward'' :: Setting -> Forward -> Aff ResultSet
executeForward'' (Local setting) query' = do
  database <- SQLite3.connect setting SQLite3.OpenReadWrite
  uri      <- insertURI query'
  _        <- SQLite3.all uri database
  _        <- SQLite3.close database
  pure (Forward unit)

executeReport :: Settings -> Report -> Aff ResultSet
executeReport (Settings settings) query' = do
  result <- Aff.sequential (Foldable.oneOf (Aff.parallel <$> flip executeReport' query' <$> settings)) 
  pure result

executeReport' :: Setting -> Report -> Aff ResultSet
executeReport' setting query' = do
  _         <- executeTouch setting
  _         <- executeSchemas setting
  result    <- executeReport'' setting query'
  pure result

executeReport'' :: Setting -> Report -> Aff ResultSet
executeReport'' (Local setting) report = do
  database   <- SQLite3.connect setting SQLite3.OpenReadOnly
  min        <- executeReport''' database (minURI report)
  max        <- executeReport''' database (maxURI report)
  sum        <- executeReport''' database (sumURI report)
  total      <- executeReport''' database (totalURI report)
  average    <- executeReport''' database (averageURI report)
  variance   <- executeReport''' database (varianceURI report average)
  _          <- SQLite3.close database
  pure $ Report $ Report.Entry $
    { min       : min
    , max       : max
    , sum       : sum
    , total     : total
    , average   : average
    , variance  : variance
    }

executeReport''' :: Connection -> String -> Aff Number
executeReport''' database uri = do
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
request settings (Route.Forward query) = liftFreeT $ (DSL.Forward settings query identity)
request settings (Route.Report query)  = liftFreeT $ (DSL.Report settings query identity)

execute ::  forall a. Request a -> Aff (Result a)
execute request' = try $ runWriterT $ runFreeT interpret request'
