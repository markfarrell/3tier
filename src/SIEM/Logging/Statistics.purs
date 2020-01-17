module SIEM.Logging.Statistics
  ( Entry(..)
  , EntryType(..)
  , statistics
  , schema
  ) where

import Prelude

import Control.Monad.Except (runExcept, throwError)
import Control.Monad.Trans.Class (lift)

import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Foldable (foldl)

import Effect.Exception (error) as Exception

import Foreign (readNumber, readString) as Foreign
import Foreign.Index ((!))

import DB as DB

sum :: String -> String -> DB.Request Number
sum filename table = do
  results <- DB.select runResult filename query
  case results of
    [sum'] -> pure sum'
    []       -> pure 0.0
    _        -> lift $ lift (throwError error)
  where
    runResult row = do
      result' <- pure (runExcept $ row ! "Total" >>= Foreign.readNumber)
      case result' of
        (Left _)       -> throwError error
        (Right sum') -> pure sum'
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
        (Left _)         -> throwError error
        (Right result'') -> pure result''
    runResult' row = do
      logID   <- row ! "LogID"   >>= Foreign.readString
      entries <- row ! "Entries" >>= Foreign.readNumber
      pure  $
        { logID   : logID
        , entries : entries }
    error = Exception.error "Unexpected results."
    query = "SELECT LogID as LogID, COUNT(DISTINCT EntryID) AS Entries FROM " <> table <> " GROUP BY LogID"

total :: String -> String -> DB.Request Number
total filename table = do
  results <- DB.select runResult filename query
  case results of
    [logs''] -> pure logs''
    _        -> lift $ lift (throwError error)
  where
    runResult row = do
       result' <- pure (runExcept $ row ! "Logs" >>= Foreign.readNumber)
       case result' of
         (Left _)          -> throwError error
         (Right logs'')    -> pure logs''
    error = Exception.error "Unexpected results."
    query = "SELECT COUNT(DISTINCT LogID) as Logs FROM " <> table

variance :: String -> String -> DB.Request Number
variance filename table = do
  results <- logs filename table
  pure $ variance' results
  where
    variance' results   = (variance'' results) / (sum'' results)
    variance'' results  = foldl (+) 0.0 $ variance''' results (average' results)
    variance''' results = \avg -> flip (<$>) results $ variance'''' avg
    variance'''' avg    = \result -> (result.entries - avg) * (result.entries - avg)
    average' results    = (sum' results) / (sum'' results)
    sum' results      = foldl (+) 0.0 (sums results)
    sum'' results     = foldl (+) 0.0 (sums'' results)
    sums results      = flip (<$>) results $ \result -> result.entries
    sums'' results    = flip (<$>) results $ \_ -> 1.0

data EntryType = LogID

instance showEntryType :: Show EntryType where
  show LogID = "LogID"

instance eqEntryType :: Eq EntryType where
  eq LogID LogID = true

data Entry = Entry
  { min                 :: Number
  , max                 :: Number
  , sum                 :: Number
  , total               :: Number
  , average             :: Number
  , variance            :: Number 
  , entryType           :: EntryType 
  }

instance showEntry :: Show Entry where
  show (Entry entry) = "(Entry " <> show entry <> ")"

instance eqEntry :: Eq Entry where
  eq (Entry x) (Entry y) = x == y

statistics :: String -> String -> DB.Request Entry
statistics filename table = do
  min'      <- minimum filename table
  max'      <- maximum filename table
  sum'      <- sum filename table
  total'    <- total filename table
  average'  <- average filename table 
  variance' <- variance filename table
  pure $ Entry $
    { min       : min'
    , max       : max'
    , sum       : sum'
    , total     : total'
    , average   : average'
    , variance  : variance'
    , entryType : LogID
    }

schema :: String -> DB.Request Unit
schema filename = DB.schema filename "Statistics" $
  [ Tuple "Timestamp" DB.TextNotNull
  , Tuple "RemoteAddress" DB.TextNotNull
  , Tuple "RemotePort" DB.TextNotNull
  , Tuple "LogID" DB.TextNotNull
  , Tuple "EntryID" DB.TextNotNull
  , Tuple "Minimum" DB.TextNotNull
  , Tuple "Maximum" DB.TextNotNull
  , Tuple "Logs" DB.TextNotNull
  , Tuple "Total"   DB.TextNotNull
  , Tuple "Average" DB.TextNotNull
  , Tuple "Variance" DB.TextNotNull
  ]
