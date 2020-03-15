module DB
 ( RequestDSL
 , Interpreter
 , Request
 , Result
 , Database
 , Table
 , ColumnType(..)
 , Schema(..)
 , close
 , connect
 , insert 
 , select
 , schema
 , touch
 , remove
 , runRequest
 ) where

import Prelude

import Control.Monad.Free.Trans (FreeT, liftFreeT, runFreeT)
import Control.Monad.Error.Class (try)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Class (tell)
import Control.Monad.Writer.Trans (WriterT, runWriterT)

import Data.Array as Array
import Data.Either (Either)
import Data.Traversable (sequence)

import Data.Tuple (Tuple(..), fst, snd)

import Effect.Aff (Aff)
import Effect.Exception (Error)

import Arrays as Arrays

import SQLite3 as SQLite3

type Database = String

type Table = String

data Schema  = Audit | Flow

instance showSchema :: Show Schema where
  show Audit = "Audit"
  show Flow  = "Flow"

data SelectType = All | Success | Failure | Duration

data RequestDSL a = Close SQLite3.Database (Unit -> a)
  | Connect Database SQLite3.Mode (SQLite3.Database -> a) 
  | Execute Table SQLite3.Database (Array SQLite3.Row -> a)

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

connect :: Database -> SQLite3.Mode -> Request SQLite3.Database
connect filename mode = liftFreeT $ (Connect filename mode identity)

all :: String -> SQLite3.Database -> Request (Array SQLite3.Row)
all query database = liftFreeT $ (Execute query database identity)

insert :: Database -> Table -> Array (Tuple String String) -> Request Unit
insert filename table' params = do
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

--select' :: Selection -> String
--select'  Audit Events    = "SELECT LogID, SourceID, EntryID, EventType, EventID, COUNT(*) as Events FROM Audit GROUP BY LogID, SourceID, EntryID"
--select'  Audit Successes = "SELECT LogID, SourceID, EntryID, EventType, EventID, COUNT(*) as Events FROM (SELECT * FROM Audit WHERE EventType='Success') GROUP BY LogID, SourceID, EntryID"
--select'  Audit Failures  = "SELECT LogID, SourceID, EntryID, EventType, EventID, COUNT(*) as Events FROM (SELECT * FROM Audit WHERE EventType='Failure') GROUP BY LogID, SourceID, EntryID"
--select'  Flow  Events    = "SELECT LogID, SourceID, EntryID, E 

select :: forall a. (SQLite3.Row -> Aff a) -> Database -> String -> Request (Array a)
select runResult filename query = do
  database <- connect filename SQLite3.OpenReadOnly
  rows     <- all query $ database
  _        <- close database
  results  <- lift (lift $ sequence (runResult <$> rows))
  lift $ pure results

type Column = Tuple String ColumnType
 
data ColumnType = Text | Real

remove :: Database -> Table -> Request Unit
remove filename table' = do
  database <- connect filename SQLite3.OpenReadWrite
  _        <- all query $ database
  _        <- close database
  lift $ pure unit
  where query = "DROP TABLE IF EXISTS " <> table'

schema' :: Database -> Table -> Array Column -> Array Column -> Request Unit
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

schema :: Schema -> Database -> Request Unit
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

touch :: Database -> Request Unit
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
 
runRequest ::  forall a. Request a -> Aff (Result a)
runRequest request = try $ runWriterT $ runFreeT interpret request
