module Statistics
  ( Entry(..)
  , ReportType(..)
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

{-- data ReportType = Events | Successes | Failures | Durations --}
data ReportType = Events

instance showReportType :: Show ReportType where
  show Events = "Events"

entries' :: DB.Table -> DB.Table
entries' table = "SELECT COUNT(DISTINCT EntryID) AS Entries FROM (" <> table <> ") GROUP BY LogID, SourceID" 

entries :: DB.Schema -> ReportType -> DB.Table
entries DB.Audit Events = entries' "Audit"
entries DB.Flow  Events = entries' "Flow"

sum :: DB.Database -> DB.Schema -> ReportType ->  DB.Request Number
sum filename schema ty = do
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
    query = "SELECT SUM(Entries) AS Result FROM (" <> (entries schema ty) <> ")"

average :: DB.Database -> DB.Schema -> ReportType ->  DB.Request Number
average filename schema ty = do
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
    query = "SELECT AVG(Entries) AS Average FROM (" <> (entries schema ty) <> ")"

minimum :: DB.Database -> DB.Schema -> ReportType -> DB.Request Number
minimum filename schema ty = do
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
    query = "SELECT MIN(Entries) AS Minimum FROM (" <> (entries schema ty) <> ")"

maximum :: DB.Database -> DB.Schema -> ReportType -> DB.Request Number
maximum filename schema ty = do
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
    query = "SELECT MAX(Entries) AS Maximum FROM (" <> (entries schema ty) <> ")"

total :: DB.Database -> DB.Schema -> ReportType -> DB.Request Number
total filename schema ty = do
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
    query = "SELECT COUNT(*) as Total FROM (" <> (entries schema ty) <> ")"

variance' :: DB.Database -> DB.Schema -> ReportType -> Number -> DB.Request Number
variance' filename schema ty = \average' -> do
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
    query average'  = "SELECT AVG(" <> query' average' <> " * " <> query' average' <> ") AS Variance FROM (" <> (entries schema ty) <> ")"
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

statistics :: DB.Database -> DB.Schema -> ReportType -> DB.Request Entry
statistics filename schema ty = do
  min'       <- minimum filename schema ty
  max'       <- maximum filename schema ty
  sum'       <- sum filename schema ty
  total'     <- total filename schema ty
  average'   <- average filename schema ty
  variance'' <- variance' filename schema ty $ average'
  pure $ Entry $
    { min       : min'
    , max       : max'
    , sum       : sum'
    , total     : total'
    , average   : average'
    , variance  : variance''
    }

report :: DB.Database -> DB.Schema -> ReportType -> DB.Request String
report filename schema ty  = do
  result  <- unparse <$> statistics filename schema ty
  case result of
    (Left _)        -> throwError $ Exception.error "Unexpected behaviour."
    (Right result') -> pure result' 

unparse :: Entry -> Either Error String
unparse (Entry entry) = pure $ stringify entry
  where stringify = JSON.stringify <<< unsafeCoerce
