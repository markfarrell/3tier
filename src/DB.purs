module DB
 ( RequestDSL
 , Interpreter
 , Request
 , Result
 , Database
 , Table
 , ColumnType(..)
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

import Data.Either (Either)
import Data.Traversable (sequence)

import Data.Tuple (Tuple, fst, snd)

import Effect.Aff (Aff)
import Effect.Exception (Error)

import Arrays as Arrays

import SQLite3 as SQLite3

type Database = String

type Table = String

data RequestDSL a = Close SQLite3.Database (Unit -> a)
  | Connect Database SQLite3.Mode (SQLite3.Database -> a) 
  | Query Table SQLite3.Database (Array SQLite3.Row -> a)

instance functorRequestDSL :: Functor RequestDSL where
  map :: forall a b. (a -> b) -> RequestDSL a -> RequestDSL b
  map f (Close database next)        = (Close database (f <<< next))
  map f (Connect filename mode next) = (Connect filename mode (f <<< next))
  map f (Query query database next)  = (Query query database (f <<< next))

type Interpreter = WriterT (Array String) Aff 

type Request a = FreeT RequestDSL Interpreter a

type Result a = Either Error (Tuple a (Array String))

close :: SQLite3.Database -> Request Unit
close database = liftFreeT $ (Close database identity)

connect :: Database -> SQLite3.Mode -> Request SQLite3.Database
connect filename mode = liftFreeT $ (Connect filename mode identity)

all :: String -> SQLite3.Database -> Request (Array SQLite3.Row)
all query database = liftFreeT $ (Query query database identity)

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

select :: forall a. (SQLite3.Row -> Aff a) -> Database -> String -> Request (Array a)
select runResult filename query = do
  database <- connect filename SQLite3.OpenReadOnly
  rows     <- all query $ database
  _        <- close database
  results  <- lift (lift $ sequence (runResult <$> rows))
  lift $ pure results

data ColumnType = TextNotNull | RealNotNull

remove :: Database -> Table -> Request Unit
remove filename table' = do
  database <- connect filename SQLite3.OpenReadWrite
  _        <- all query $ database
  _        <- close database
  lift $ pure unit
  where query = "DROP TABLE IF EXISTS " <> table'

schema :: Database -> Table -> Array (Tuple String ColumnType) -> Request Unit
schema filename table' params = do
  database <- connect filename SQLite3.OpenReadWrite
  _        <- all query $ database
  _        <- close database
  lift $ pure unit
  where
     query = "CREATE TABLE IF NOT EXISTS " <> table' <> " (" <> columns <> ")"
     columns                = (Arrays.join "," columns')
     columns'               = column <$> params
     column param           = Arrays.join " " $ [fst param, columnType $ snd param]
     columnType TextNotNull = "TEXT NOT NULL"
     columnType RealNotNull = "REAL NOT NULL"

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
interpret (Query query database next) = do
  _      <- tell ["QUERY"]
  result <- lift $ next <$> SQLite3.all query database
  lift $ pure result
 
runRequest ::  forall a. Request a -> Aff (Result a)
runRequest request = try $ runWriterT $ runFreeT interpret request
