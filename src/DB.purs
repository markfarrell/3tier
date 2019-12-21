module DB
 ( RequestDSL
 , Interpreter
 , Request
 , Result
 , close
 , connect
 , insert 
 , select
 , runRequest
 ) where

import Prelude

import Control.Monad.Except (runExcept)
import Control.Monad.Free.Trans (FreeT, liftFreeT, runFreeT)
import Control.Monad.Error.Class (try)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer.Class (tell)
import Control.Monad.Writer.Trans (WriterT, runWriterT)

import Data.Either (Either, either)
import Data.Traversable (sequence)
import Data.Tuple (Tuple)

import Effect.Aff (Aff)
import Effect.Exception (Error)

import SQLite3 as SQLite3

data RequestDSL a = Close SQLite3.Database (Unit -> a)
  | Connect String SQLite3.Mode (SQLite3.Database -> a) 
  | All String SQLite3.Database (Array SQLite3.Row -> a)

instance functorRequestDSL :: Functor RequestDSL where
  map :: forall a b. (a -> b) -> RequestDSL a -> RequestDSL b
  map f (Close database next)        = (Close database (f <<< next))
  map f (Connect filename mode next) = (Connect filename mode (f <<< next))
  map f (All query database next) = (All query database (f <<< next))

type Interpreter = WriterT (Array String) Aff 

type Request a = FreeT RequestDSL Interpreter a

type Result a = Either Error (Tuple a (Array String))

close :: SQLite3.Database -> Request Unit
close database = liftFreeT $ (Close database identity)

connect :: String -> SQLite3.Mode -> Request SQLite3.Database
connect filename mode = liftFreeT $ (Connect filename mode identity)

all :: String -> SQLite3.Database -> Request (Array SQLite3.Row)
all query database = liftFreeT $ (All query database identity)

insert :: String -> String -> Request Unit
insert filename query = do
  database <- connect filename SQLite3.OpenReadWrite
  _        <- all query $ database
  _        <- close database
  lift $ pure unit

select :: forall a. String -> String -> (SQLite3.Row -> SQLite3.Result a) -> Request (Array a)
select filename query readResult = do
  database <- connect filename SQLite3.OpenReadWrite
  rows     <- all query $ database
  _        <- close database
  lift $ pure (resultSet rows)
  where
    resultSet  rows = either (const []) identity $ resultSet' rows
    resultSet' rows = sequence $ runExcept <$> readResult <$> rows

interpret :: forall a. RequestDSL (Request a) -> Interpreter (Request a)
interpret (Close database next) = do 
  _      <- tell ["CLOSE"]
  result <- lift $ next <$> SQLite3.close database
  lift $ pure result
interpret (Connect filename mode next) = do
  _      <- tell ["CONNECT " <> filename <> " " <> show mode]
  result <- lift $ next <$> SQLite3.connect filename mode
  lift $ pure result 
interpret (All query database next) = do
  _      <- tell ["ALL " <> query]
  result <- lift $ next <$> SQLite3.all query database
  lift $ pure result
 
runRequest ::  forall a. Request a -> Aff (Result a)
runRequest request = try $ runWriterT $ runFreeT interpret request
