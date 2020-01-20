module SIEM.Logging.Statistics
  ( Entry(..)
  , EntryType(..)
  , statistics
  , report
  , schema
  ) where

import Prelude

import Control.Monad.Except (runExcept, throwError)
import Control.Monad.Trans.Class (lift)

import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Foldable (foldl)
import Data.Traversable (sequence)

import Effect.Exception (error) as Exception

import Foreign (readNumber) as Foreign
import Foreign.Index ((!))

import Unsafe.Coerce (unsafeCoerce)

import JSON as JSON

import DB as DB

entryType :: EntryType -> String
entryType LogID         = "LogID"
entryType RemoteAddress = "RemoteAddress"

sum :: String -> String -> EntryType -> DB.Request Number
sum filename table ty = do
  results <- DB.select runResult filename query
  case results of
    [sum']   -> pure sum'
    _        -> lift $ lift (throwError error)
  where
    runResult row = do
      result' <- pure (runExcept $ row ! "Total" >>= Foreign.readNumber)
      case result' of
        (Left _)     -> pure 0.0
        (Right sum') -> pure sum'
    error = Exception.error "Unexpected results."
    query = "SELECT SUM(Entries) AS Total FROM (SELECT COUNT(DISTINCT EntryID) AS Entries FROM " <> table <> " GROUP BY " <> (entryType ty) <> ")"

average :: String -> String -> EntryType -> DB.Request Number
average filename table ty = do
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
    query = "SELECT AVG(Entries) AS Average FROM (SELECT COUNT(DISTINCT EntryID) AS Entries FROM " <> table <> " GROUP BY " <> (entryType ty) <> ")"

minimum :: String -> String -> EntryType -> DB.Request Number
minimum filename table ty = do
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
    query = "SELECT MIN(Entries) AS Minimum FROM (SELECT COUNT(DISTINCT EntryID) AS Entries FROM " <> table <> " GROUP BY " <> (entryType ty) <> ")"

maximum :: String -> String -> EntryType -> DB.Request Number
maximum filename table ty = do
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
    query = "SELECT MAX(Entries) AS Maximum FROM (SELECT COUNT(DISTINCT EntryID) AS Entries FROM " <> table <> " GROUP BY " <> (entryType ty) <> ")"


totals :: String -> String -> EntryType -> DB.Request (Array Number)
totals filename table ty = DB.select runResult filename query
  where
    runResult row = do
      result' <- pure (runExcept $ runResult' row)
      case result' of
        (Left _)         -> pure 0.0
        (Right result'') -> pure result''
    runResult' row = do
      total' <- row ! "Total"  >>= Foreign.readNumber
      pure total'
    query = "SELECT COUNT(DISTINCT EntryID) AS Total FROM " <> table <> " GROUP BY " <> (entryType ty)

total :: String -> String -> EntryType -> DB.Request Number
total filename table ty = do
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
    query = "SELECT COUNT(DISTINCT " <> (entryType ty) <> ") as Total FROM " <> table

variance :: String -> String -> EntryType -> DB.Request Number
variance filename table ty = do
  results <- totals filename table $ ty
  case results of
    _     -> pure $ variance' results
  where
    variance' results   = (variance'' results) / (sum'' results)
    variance'' results  = foldl (+) 0.0 $ variance''' results (average' results)
    variance''' results = \avg -> flip (<$>) results $ variance'''' avg
    variance'''' avg    = \result -> (result - avg) * (result - avg)
    average' results    = (sum' results) / (sum'' results)
    sum' results      = foldl (+) 0.0 results
    sum'' results     = foldl (+) 0.0 (sum''' results)
    sum''' results    = flip (<$>) results $ \_ -> 1.0

data EntryType = LogID | RemoteAddress

instance showEntryType :: Show EntryType where
  show LogID         = "LogID"
  show RemoteAddress = "RemoteAddress"

instance eqEntryType :: Eq EntryType where
  eq LogID LogID                 = true
  eq RemoteAddress RemoteAddress = true
  eq _ _                         = false

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

statistics :: String -> String -> EntryType -> DB.Request Entry
statistics filename table ty = do
  min'      <- minimum filename table  $ ty
  max'      <- maximum filename table  $ ty
  sum'      <- sum filename table      $ ty
  total'    <- total filename table    $ ty
  average'  <- average filename table  $ ty 
  variance' <- variance filename table $ ty
  pure $ Entry $
    { min       : min'
    , max       : max'
    , sum       : sum'
    , total     : total'
    , average   : average'
    , variance  : variance'
    }

report :: String -> DB.Request String
report filename = do
    result  <- sequence (report' LogID <$> tables)
    result' <- sequence (report' RemoteAddress <$> tables)
    pure $ stringify (result <> result')
  where
    report' ty table = do
      result  <- statistics filename table ty
      case result of
        (Entry entry) -> pure $ 
          { entryClass       : table
          , entryType        : (entryType ty)
          , statistics       : entry
          }
    stringify = JSON.stringify <<< unsafeCoerce
    tables    = ["Sensor", "Windows", "Linux", "Audit"]

schema :: String -> DB.Request Unit
schema filename = DB.schema filename "Statistics" $
  [ Tuple "Timestamp" DB.TextNotNull
  , Tuple "RemoteAddress" DB.TextNotNull
  , Tuple "RemotePort" DB.TextNotNull
  , Tuple "LogID" DB.TextNotNull
  , Tuple "EntryID" DB.TextNotNull
  , Tuple "ReportClass" DB.TextNotNull
  , Tuple "ReportType" DB.TextNotNull 
  , Tuple "Minimum" DB.TextNotNull
  , Tuple "Maximum" DB.TextNotNull
  , Tuple "Sum" DB.TextNotNull
  , Tuple "Total"   DB.TextNotNull
  , Tuple "Average" DB.TextNotNull
  , Tuple "Variance" DB.TextNotNull
  ]
