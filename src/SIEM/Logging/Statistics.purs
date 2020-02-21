module SIEM.Logging.Statistics
  ( Entry(..)
  , statistics
  , report
  , schema
  , writeEntry
  ) where

import Prelude

import Control.Monad.Except (runExcept, throwError)
import Control.Monad.Trans.Class (lift)

import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Foldable (foldl)

import Effect.Exception (Error)
import Effect.Exception (error) as Exception

import Foreign (readNumber) as Foreign
import Foreign.Index ((!))

import Unsafe.Coerce (unsafeCoerce)
import JSON as JSON

import DB as DB

entries :: String -> String
entries table = "SELECT COUNT(DISTINCT EntryID) AS Entries FROM " <> table <> " GROUP BY LogID, RemoteAddress" 

sum :: String -> String -> DB.Request Number
sum filename table = do
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
    query = "SELECT SUM(Entries) AS Total FROM (" <> (entries table) <> ")"

average :: String -> String -> DB.Request Number
average filename table = do
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
    query = "SELECT AVG(Entries) AS Average FROM (" <> (entries table) <> ")"

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
        (Left _)         -> pure 0.0
        (Right min')     -> pure min'
    error = Exception.error "Unexpected results."
    query = "SELECT MIN(Entries) AS Minimum FROM (" <> (entries table) <> ")"

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
        (Left _)         -> pure 0.0
        (Right max')     -> pure max'
    error = Exception.error "Unexpected results."
    query = "SELECT MAX(Entries) AS Maximum FROM (" <> (entries table) <> ")"

totals :: String -> String -> DB.Request (Array Number)
totals filename table = DB.select runResult filename query
  where
    runResult row = do
      result' <- pure (runExcept $ runResult' row)
      case result' of
        (Left _)         -> pure 0.0
        (Right result'') -> pure result''
    runResult' row = do
      total' <- row ! "Entries"  >>= Foreign.readNumber
      pure total'
    query = entries table

total :: String -> String -> DB.Request Number
total filename table = do
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
    query = "SELECT COUNT(*) as Total FROM (" <> (entries table) <> ")"

variance :: String -> String -> DB.Request Number
variance filename table = do
  results <- totals filename table
  case results of
    _     -> pure $ variance' results
  where
    variance' results   = divide' (variance'' results) (sum'' results)
    variance'' results  = foldl (+) 0.0 $ variance''' results (average' results)
    variance''' results = \avg -> flip (<$>) results $ variance'''' avg
    variance'''' avg    = \result -> (result - avg) * (result - avg)
    average' results    = divide' (sum' results) (sum'' results)
    sum' results      = foldl (+) 0.0 results
    sum'' results     = foldl (+) 0.0 (sum''' results)
    sum''' results    = flip (<$>) results $ \_ -> 1.0
    divide' x y       = if y == 0.0 then 0.0 else x / y 

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
    }

report :: String -> String -> DB.Request String
report filename table = do
  result  <- writeEntry <$> statistics filename table
  case result of
    (Left _)        -> throwError $ Exception.error "Unexpected behaviour."
    (Right result') -> pure result' 

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

writeEntry :: Entry -> Either Error String
writeEntry (Entry entry) = pure $ stringify entry
  where stringify = JSON.stringify <<< unsafeCoerce
