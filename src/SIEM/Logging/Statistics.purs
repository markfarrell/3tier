module SIEM.Logging.Statistics
  ( Entry(..)
  , statistics
  ) where

import Prelude

import Control.Monad.Except (runExcept, throwError)
import Control.Monad.Trans.Class (lift)

import Data.Either (Either(..))
import Data.Foldable (foldl)

import Effect.Exception (error) as Exception

import Foreign (readNumber, readString) as Foreign
import Foreign.Index ((!))

import DB as DB

total :: String -> String -> DB.Request Number
total filename table = do
  results <- DB.select runResult filename query
  case results of
    [total'] -> pure total'
    []       -> pure 0.0
    _        -> lift $ lift (throwError error)
  where
    runResult row = do
      result' <- pure (runExcept $ row ! "Total" >>= Foreign.readNumber)
      case result' of
        (Left _)       -> throwError error
        (Right total') -> pure total'
    error = Exception.error "Unexpected results."
    query = "SELECT SUM(Entries) AS Total FROM (SELECT COUNT(DISTINCT EntryID) AS Entries FROM " <> table <> " GROUP BY LogID)"

average :: String -> String -> DB.Request Number
average filename table = do
  results <- DB.select runResult filename query
  case results of
    [average'] -> pure average'
    _          -> lift $ lift (throwError error)
  where
    runResult row = do
      result' <- pure (runExcept $ row ! "Average" >>= Foreign.readNumber)
      case result' of
        (Left _)         -> throwError error
        (Right average') -> pure average'
    error = Exception.error "Unexpected results."
    query = "SELECT AVG(Entries) AS Average FROM (SELECT COUNT(DISTINCT EntryID) AS Entries FROM " <> table <> " GROUP BY LogID)"

minimum :: String -> String -> DB.Request Number
minimum filename table = do
  results <- DB.select runResult filename query
  case results of
    [min'] -> pure min'
    _      -> lift $ lift (throwError error)
  where
    runResult row = do
      result' <- pure (runExcept $ row ! "Minimum" >>= Foreign.readNumber)
      case result' of
        (Left _)         -> throwError error
        (Right min')     -> pure min'
    error = Exception.error "Unexpected results."
    query = "SELECT MIN(Entries) AS Minimum FROM (SELECT COUNT(DISTINCT EntryID) AS Entries FROM " <> table <> " GROUP BY LogID)"

maximum :: String -> String -> DB.Request Number
maximum filename table = do
  results <- DB.select runResult filename query
  case results of
    [max'] -> pure max'
    _      -> lift $ lift (throwError error)
  where
    runResult row = do
      result' <- pure (runExcept $ row ! "Maximum" >>= Foreign.readNumber)
      case result' of
        (Left _)         -> throwError error
        (Right max')     -> pure max'
    error = Exception.error "Unexpected results."
    query = "SELECT MAX(Entries) AS Maximum FROM (SELECT COUNT(DISTINCT EntryID) AS Entries FROM " <> table <> " GROUP BY LogID)"


logs :: String -> String -> DB.Request (Array { logID :: String, entries :: Number })
logs filename table = DB.select runResult filename query
  where
    runResult row = do
      result' <- pure (runExcept $ runResult' row)
      case result' of
        (Left _)       -> throwError error
        (Right total') -> pure total'
    runResult' row = do
      logID   <- row ! "LogID"   >>= Foreign.readString
      entries <- row ! "Entries" >>= Foreign.readNumber
      pure  $
        { logID   : logID
        , entries : entries }
    error = Exception.error "Unexpected results."
    query = "SELECT LogID as LogID, COUNT(DISTINCT EntryID) AS Entries FROM " <> table <> " GROUP BY LogID"

variance :: String -> String -> DB.Request Number
variance filename table = do
  results <- logs filename table
  pure $ variance' results
  where
    variance' results   = (variance'' results) / (total'' results)
    variance'' results  = foldl (+) 0.0 $ variance''' results (average' results)
    variance''' results = \avg -> flip (<$>) results $ variance'''' avg
    variance'''' avg    = \result -> (result.entries - avg) * (result.entries - avg)
    average' results    = (total' results) / (total'' results)
    total' results      = foldl (+) 0.0 (totals results)
    total'' results     = foldl (+) 0.0 (totals'' results)
    totals results      = flip (<$>) results $ \result -> result.entries
    totals'' results    = flip (<$>) results $ \_ -> 1.0

data Entry = Entry
  { min          :: Number
  , max          :: Number
  , total        :: Number
  , average      :: Number
  , variance     :: Number 
  }

instance showEntry :: Show Entry where
  show (Entry entry) = "(Entry " <> show entry <> ")"

instance eqEntry :: Eq Entry where
  eq (Entry x) (Entry y) = x == y

statistics :: String -> String -> DB.Request Entry
statistics filename table = do
  min'      <- minimum filename table
  max'      <- maximum filename table
  total'    <- total filename table
  average'  <- average filename table 
  variance' <- variance filename table
  pure $ Entry $
    { min      : min'
    , max      : max'
    , total    : total'
    , average  : average'
    , variance : variance'
    }
