module DB.Report
  ( Entry(..)
  , ReportType(..)
  , Report(..)
  , select
  ) where

import Prelude

import Control.Monad.Except (runExcept, throwError)
import Control.Monad.Trans.Class (lift)

import Data.Either (Either(..))

import Effect.Exception as Exception

import Foreign (readNumber) as Foreign
import Foreign.Index ((!))

import Audit as Audit

import DB as DB

data ReportType = Events | Durations

data Report = Audit Audit.EventID Audit.EventType ReportType

instance showReportType :: Show ReportType where
  show Events    = "Events"
  show Durations = "Durations"

instance showReport :: Show Report where
  show (Audit eventID eventType reportType) = "(Audit " <> show eventID <> " " <> show eventType <> " " <> show reportType <> ")" 

sample' :: ReportType -> DB.Table -> DB.Table
sample' Events    = \table -> "SELECT COUNT(DISTINCT EntryID) AS X FROM (" <> table <> ") GROUP BY LogID, SourceID" 
sample' Durations = \table -> "SELECT Duration as X FROM (" <> table <> ")"

sample :: Report -> DB.Table
sample (Audit eventID eventType reportType) = sample' reportType $ "SELECT * FROM Audit WHERE EventID='" <> show eventID <> "' AND EventType='" <> show eventType <> "'"

sum :: DB.Database -> Report -> DB.Request Number
sum filename report = do
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
    query = "SELECT SUM(X) AS Result FROM (" <> (sample report) <> ")"

average :: DB.Database -> Report ->  DB.Request Number
average filename report = do
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
    query = "SELECT AVG(X) AS Average FROM (" <> (sample report) <> ")"

minimum :: DB.Database -> Report -> DB.Request Number
minimum filename report = do
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
    query = "SELECT MIN(X) AS Minimum FROM (" <> (sample report) <> ")"

maximum :: DB.Database -> Report -> DB.Request Number
maximum filename report = do
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
    query = "SELECT MAX(X) AS Maximum FROM (" <> (sample report) <> ")"

total :: DB.Database -> Report -> DB.Request Number
total filename report = do
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
    query = "SELECT COUNT(*) as Total FROM (" <> (sample report) <> ")"

variance' :: DB.Database -> Report -> Number -> DB.Request Number
variance' filename report = \average' -> do
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
    query average'  = "SELECT AVG(" <> query' average' <> " * " <> query' average' <> ") AS Variance FROM (" <> (sample report) <> ")"
    query' average' = "(X - " <> show average' <> ")"

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

select :: DB.Database -> Report -> DB.Request Entry
select filename report = do
  min'       <- minimum filename report
  max'       <- maximum filename report
  sum'       <- sum filename report
  total'     <- total filename report
  average'   <- average filename report
  variance'' <- variance' filename report $ average'
  pure $ Entry $
    { min       : min'
    , max       : max'
    , sum       : sum'
    , total     : total'
    , average   : average'
    , variance  : variance''
    }
