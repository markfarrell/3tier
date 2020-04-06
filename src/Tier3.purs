module Tier3
 ( DSL
 , Query
 , Request
 , Result
 , DBMS(..)
 , Authorization(..)
 , Authentication(..)
 , Settings(..)
 , Resource(..)
 , request
 , execute
 ) where

import Prelude

import Control.Monad.Except (runExcept, throwError)
import Control.Monad.Free.Trans (liftFreeT, runFreeT)
import Control.Monad.Error.Class (try)

import Data.Either (Either(..))
import Data.Traversable (sequence)

import Data.Foldable (intercalate)
import Data.Foldable (oneOf) as Foldable

import Data.Tuple (Tuple(..), fst, snd)

import Effect.Aff (Aff)
import Effect.Aff (parallel, sequential) as Aff
import Effect.Exception as Exception

import Foreign (readNumber) as Foreign
import Foreign.Index ((!))

import FFI.SQLite3 as SQLite3

import Data.Audit as Audit
import Data.Flow as Flow
import Data.Windows as Windows

import Data.Schema (Schema)
import Data.Schema as Schema

import Forward (Forward)
import Forward as Forward

import Report (Report)
import Report as Report

import Route (Route)
import Route as Route

import DSL as DSL

type Connection = SQLite3.Database

data DBMS = Local String | Replication (Array DBMS)

data Authorization = Authorization Unit

data Authentication = Authentication Unit

data Settings = Settings Authorization Authentication DBMS

type Table = String

type Column = Tuple String ColumnType
 
data ColumnType = Text | Integer

data Resource = Forward Unit | Report Report.Event

type Query = Route

type DSL a = DSL.DSL Settings Resource a

type Request a = DSL.Request Settings Resource a

type Result a = DSL.Result a

schemaURI' :: Table -> Array Column -> Array Column -> String
schemaURI' table' params params' = query
  where
     query              = "CREATE TABLE IF NOT EXISTS " <> table' <> " (" <> columns <> ")"
     columns            = (intercalate "," columns')
     columns'           = column <$> (params <> params')
     column param       = intercalate " " $ [fst param, columnType $ snd param]
     columnType Text    = "TEXT NOT NULL"
     columnType Integer = "INTEGER NOT NULL"

schemaURI :: Schema -> String
schemaURI Schema.Audit = schemaURI' "Audit" [] $
  [ Tuple "SIP" Text
  , Tuple "SPort" Text
  , Tuple "StartTime" Text
  , Tuple "Duration" Integer
  , Tuple "EndTime" Text
  , Tuple "EventType" Text
  , Tuple "EventCategory" Text
  , Tuple "EventID" Text
  ]
schemaURI Schema.Flow = schemaURI' "Flow" [] $ 
  [ Tuple "SIP" Text
  , Tuple "DIP" Text
  , Tuple "SPort" Integer
  , Tuple "DPort" Integer
  , Tuple "Protocol" Integer
  , Tuple "Packets" Integer
  , Tuple "Bytes" Integer
  , Tuple "Flags" Text
  , Tuple "StartTIme" Text
  , Tuple "Duration" Integer
  , Tuple "EndTime" Text
  ]
schemaURI Schema.Windows = schemaURI' "Windows" [] $
  [ Tuple "StartTime" Text
  , Tuple "Duration" Integer
  , Tuple "EndTime" Text
  , Tuple "EventType" Text
  , Tuple "EventCategory" Text
  , Tuple "EventID" Text
  ]

insertAuditURI :: Audit.Event -> Aff String
insertAuditURI (Audit.Event event) = do
  pure $ insertURI' "Audit" params
  where 
    params  =
      [ Tuple "SIP" event.sIP
      , Tuple "SPort" (show event.sPort)
      , Tuple "StartTime" (show event.startTime)
      , Tuple "Duration" (show event.duration)
      , Tuple "EndTime" (show event.endTime)
      , Tuple "EventType" (show event.eventType)
      , Tuple "EventCategory" (show event.eventCategory)
      , Tuple "EventID" (show event.eventID)
      ]

insertFlowURI :: Flow.Event -> Aff String
insertFlowURI (Flow.Event event) = do
  pure $ insertURI' "Flow" params
  where
    params = 
      [ Tuple "SIP" (show event.sIP)
      , Tuple "DIP" (show event.dIP)
      , Tuple "SPort" (show event.sPort)
      , Tuple "DPort" (show event.dPort)
      , Tuple "Protocol" (show event.protocol)
      , Tuple "Packets" (show event.packets)
      , Tuple "Bytes" (show event.bytes)
      , Tuple "Flags" (intercalate "" (show <$> event.flags))
      , Tuple "StartTIme" (show event.startTime)
      , Tuple "Duration" (show event.duration)
      , Tuple "EndTime" (show event.endTime)
      ]

insertWindowsURI :: Windows.Event -> Aff String
insertWindowsURI (Windows.Event event) = do
  pure $ insertURI' "Windows" params
  where 
    params  =
      [ Tuple "StartTime" (show event.startTime)
      , Tuple "Duration" (show event.duration)
      , Tuple "EndTime" (show event.endTime)
      , Tuple "EventType" (show event.eventType)
      , Tuple "EventCategory" (show event.eventCategory)
      , Tuple "EventID" (show event.eventID)
      ]


insertURI' ::  Table -> Array (Tuple String String) -> String
insertURI'  table' params = query
  where
     query = "INSERT INTO " <> table' <> " (" <> columns <> ") VALUES (" <> values <> ")"
     columns  = "'" <> (intercalate "','" columns') <> "'"
     values   = "'" <> (intercalate "','" values') <> "'"
     columns' = fst <$> params
     values'  = snd <$> params

insertURI :: Forward ->  Aff String
insertURI (Forward.Audit event)    = insertAuditURI event
insertURI (Forward.Flow  event)    = insertFlowURI event
insertURI (Forward.Windows event)  = insertWindowsURI event


reportAuditURI' :: Audit.ReportType -> Table -> Table
reportAuditURI' Audit.Source   = \table -> "SELECT COUNT(*) AS X FROM (" <> table <> ") GROUP BY SIP, SPort" 
reportAuditURI' Audit.Duration = \table -> "SELECT Duration as X FROM (" <> table <> ")"

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

interpret :: forall a. DSL (Request a) -> Aff (Request a)
interpret (DSL.Forward (Settings _ _ dbms) query next) = do
  result <- executeForward dbms query
  next <$> pure result
interpret (DSL.Report (Settings _ _ dbms) query next) = do
  result <- executeReport dbms query
  next <$> (pure result)

executeTouch :: String -> Aff Unit
executeTouch file = do
  database <- SQLite3.connect file SQLite3.OpenCreate
  _        <- SQLite3.close database
  pure unit

executeSchemas :: String -> Aff Unit
executeSchemas file = do
  database <- SQLite3.connect file SQLite3.OpenReadWrite
  _        <- SQLite3.all (schemaURI Schema.Audit) database
  _        <- SQLite3.all (schemaURI Schema.Flow) database
  _        <- SQLite3.all (schemaURI Schema.Windows) database
  _        <- SQLite3.close database
  pure unit

executeForward :: DBMS -> Forward -> Aff Resource
executeForward (Replication dbms) query  = do
  result <- Aff.sequential (Foldable.oneOf (Aff.parallel <$> flip executeForward query <$> dbms)) 
  pure result
executeForward (Local dbms) query        = do
  _        <- executeTouch dbms
  _        <- executeSchemas dbms
  database <- SQLite3.connect dbms SQLite3.OpenReadWrite
  uri      <- insertURI query
  _        <- SQLite3.all uri database
  _        <- SQLite3.close database
  pure (Forward unit)

executeReport :: DBMS -> Report -> Aff Resource
executeReport (Replication dbms) report = do
  result <- Aff.sequential (Foldable.oneOf (Aff.parallel <$> flip executeReport report <$> dbms))
  pure result
executeReport (Local dbms) report = do
  _          <- executeTouch dbms
  _          <- executeSchemas dbms
  database   <- SQLite3.connect dbms SQLite3.OpenReadOnly
  min        <- executeReport'' database (minURI report)
  max        <- executeReport'' database (maxURI report)
  sum        <- executeReport'' database (sumURI report)
  total      <- executeReport'' database (totalURI report)
  average    <- executeReport'' database (averageURI report)
  variance   <- executeReport'' database (varianceURI report average)
  _          <- SQLite3.close database
  pure $ Report $ Report.Event $
    { min       : min
    , max       : max
    , sum       : sum
    , total     : total
    , average   : average
    , variance  : variance
    }

executeReport'' :: Connection -> String -> Aff Number
executeReport'' database uri = do
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

request :: Settings -> Query -> Request Resource
request settings (Route.Forward query) = liftFreeT $ (DSL.Forward settings query identity)
request settings (Route.Report query)  = liftFreeT $ (DSL.Report settings query identity)

execute ::  forall a. Request a -> Aff (Result a)
execute request' = try $ runFreeT interpret request'
