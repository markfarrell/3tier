module Statistics
  ( Entry(..)
  , statistics
  , report
  , unparse
  ) where

import Prelude

import Control.Monad.Except (runExcept, throwError)
import Control.Monad.Trans.Class (lift)

import Data.Either (Either(..))

import Effect.Exception (Error)
import Effect.Exception (error) as Exception

import Foreign (readNumber) as Foreign
import Foreign.Index ((!))

import Unsafe.Coerce (unsafeCoerce)
import JSON as JSON

import DB as DB

entries' :: DB.Table -> DB.Table
entries' table = "SELECT COUNT(DISTINCT EntryID) AS Entries FROM (" <> table <> ") GROUP BY LogID, SourceID" 

entries :: DB.Schema -> DB.Table
entries DB.Audit = entries' "Audit"
entries DB.Flow  = entries' "Flow"

sum :: DB.Database -> DB.Schema -> DB.Request Number
sum filename schema = do
  results <- DB.select runResult filename query
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
    query = "SELECT SUM(Entries) AS Result FROM (" <> (entries schema) <> ")"

average :: DB.Database -> DB.Schema -> DB.Request Number
average filename schema = do
  results <- DB.select runResult filename query
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
    query = "SELECT AVG(Entries) AS Average FROM (" <> (entries schema) <> ")"

minimum :: DB.Database  -> DB.Schema -> DB.Request Number
minimum filename schema = do
  results <- DB.select runResult filename query
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
    query = "SELECT MIN(Entries) AS Minimum FROM (" <> (entries schema) <> ")"

maximum :: DB.Database -> DB.Schema -> DB.Request Number
maximum filename schema = do
  results <- DB.select runResult filename query
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
    query = "SELECT MAX(Entries) AS Maximum FROM (" <> (entries schema) <> ")"

total :: DB.Database  -> DB.Schema -> DB.Request Number
total filename schema = do
  results <- DB.select runResult filename query
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
    query = "SELECT COUNT(*) as Total FROM (" <> (entries schema) <> ")"

variance' :: DB.Database  -> DB.Schema -> Number -> DB.Request Number
variance' filename schema = \average' -> do
  results <- DB.select runResult filename $ query average'
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
    query average'  = "SELECT AVG(" <> query' average' <> " * " <> query' average' <> ") AS Variance FROM (" <> (entries schema) <> ")"
    query' average' = "(Entries - " <> show average' <> ")"

data Entry = Entry
  { min                 :: Number
  , max                 :: Number
  , sum                 :: Number
  , total               :: Number
  , average             :: Number
  , variance            :: Number 
  }

instance showEntry :: Show Entry where
  show (Entry entry) = "(Entry " <> show entry <> ")"

instance eqEntry :: Eq Entry where
  eq (Entry x) (Entry y) = x == y

statistics :: DB.Database  -> DB.Schema -> DB.Request Entry
statistics filename schema = do
  min'       <- minimum filename schema
  max'       <- maximum filename schema
  sum'       <- sum filename schema
  total'     <- total filename schema
  average'   <- average filename schema
  variance'' <- variance' filename schema $ average'
  pure $ Entry $
    { min       : min'
    , max       : max'
    , sum       : sum'
    , total     : total'
    , average   : average'
    , variance  : variance''
    }

report :: DB.Database -> DB.Schema-> DB.Request String
report filename table = do
  result  <- unparse <$> statistics filename table
  case result of
    (Left _)        -> throwError $ Exception.error "Unexpected behaviour."
    (Right result') -> pure result' 

unparse :: Entry -> Either Error String
unparse (Entry entry) = pure $ stringify entry
  where stringify = JSON.stringify <<< unsafeCoerce
