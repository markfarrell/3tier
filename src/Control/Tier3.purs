module Control.Tier3
 ( Query
 , Request
 , Result
 , Target(..)
 , Settings(..)
 , Resource(..)
 , Role(..)
 , URI(..)
 , request
 , execute
 ) where

import Prelude

import Control.Monad.Except (runExcept, throwError)
import Control.Monad.Free.Trans (liftFreeT, runFreeT)
import Control.Monad.Error.Class (try)
import Control.Monad.Trans.Class (lift)

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)

import Data.Foldable (foldl, intercalate)
import Data.Foldable (oneOf) as Foldable

import Data.Tuple (Tuple(..), fst, snd)

import Effect.Aff (Aff)
import Effect.Aff (parallel, sequential) as Aff
import Effect.Class (liftEffect)
import Effect.Exception as Exception

import Foreign (readNumber) as Foreign
import Foreign.Index ((!))

import Text.Parsing.Parser (runParser)

import FFI.Date (Date)
import FFI.Date    as Date
import FFI.Math    as Math
import FFI.SQLite3 as SQLite3
import FFI.UUID    as UUID

import Control.DSL as DSL

import Data.Audit as Audit
import Data.Event as Event
import Data.Flow (Event(..)) as Flow
import Data.Statistics as Statistics
import Data.Windows as Windows

import Data.Schema (Schema)
import Data.Schema as Schema

import Control.Authorization (Authorization)
import Control.Authentication (Authentication)

import Data.Route (Route)
import Data.Route as Route

import Data.Forward as Forward
import Data.Report as Report

import Text.Parsing.Flow (event) as Flow

data Role = Production | Testing

data URI = Primary Role | Secondary Role | Offsite Role

type Connection = SQLite3.Database

data Target = Single URI | Replication URI | Failover URI

data Settings = Settings Authorization Authentication Target

type Table = String

type Column = Tuple String ColumnType
 
data ColumnType = Text | Integer

data Resource = Forward Unit | Report Statistics.Event

type Query a = DSL.Query Settings Resource Forward.URI Report.URI a

type Request a = DSL.Request Settings Resource Forward.URI Report.URI a

type Result a = DSL.Result a

schemaURI' :: Table -> Array Column -> Array Column -> String
schemaURI' table' params params' = query
  where
     query              = "CREATE TABLE IF NOT EXISTS " <> table' <> " (" <> columns <> ")"
     columns            = (intercalate "," columns')
     columns'           = column <$> (params <> params')
     column param       = intercalate " " $ [fst param, columnType $ snd param]
     columnType Text    = "TEXT NOT NULL"
     columnType Integer = "INTEGER NOT NULL"

schemaURI :: Schema -> String
schemaURI Schema.Audit = schemaURI' "Audit" [] $
  [ Tuple "EventCategory" Text
  , Tuple "EventID" Text
  , Tuple "EventType" Text
  , Tuple "EventSource" Text
  , Tuple "EventURI" Text
  , Tuple "StartTime" Text
  , Tuple "Duration" Integer
  , Tuple "EndTime" Text
  ]
schemaURI Schema.Flow = schemaURI' "Flow" [] $ 
  [ Tuple "SIP" Text
  , Tuple "DIP" Text
  , Tuple "SPort" Integer
  , Tuple "DPort" Integer
  , Tuple "Protocol" Integer
  , Tuple "Packets" Integer
  , Tuple "Bytes" Integer
  , Tuple "Flags" Text
  , Tuple "StartTIme" Text
  , Tuple "Duration" Integer
  , Tuple "EndTime" Text
  ]
schemaURI Schema.Windows = schemaURI' "Windows" [] $
  [ Tuple "EventCategory" Text
  , Tuple "EventType" Text
  , Tuple "EventID" Text
  , Tuple "EventURI" Text
  , Tuple "EventSource" Text
  , Tuple "StartTime" Text
  , Tuple "Duration" Integer
  , Tuple "EndTime" Text
  ]
schemaURI Schema.Statistics = schemaURI' "Statistics" [] $
  [ Tuple "EventType" Text
  , Tuple "EventCategory" Text
  , Tuple "EventID" Text
  , Tuple "Min" Text
  , Tuple "Max" Text
  , Tuple "Sum" Text
  , Tuple "Total" Text
  , Tuple "Average" Text
  , Tuple "Variance" Text
  ]

startTime :: Event.Time -> Date
startTime (Event.Time x) = x.startTime

duration :: Event.Time -> Int
duration (Event.Time x) = x.duration

endTime :: Event.Time -> Date
endTime (Event.Time x) = x.endTime

insertAuditURI :: Audit.Event -> Aff String
insertAuditURI (Audit.Event event) = do
  pure $ insertURI' "Audit" params
  where 
    params  =
      [ Tuple "EventCategory" (show event.eventCategory)
      , Tuple "EventType" (show event.eventCategory)
      , Tuple "EventID" (show event.eventID)
      , Tuple "EventSource" (show event.eventSource)
      , Tuple "EventURI" (show event.eventURI)
      , Tuple "StartTime" (show $ startTime event.eventTime)
      , Tuple "Duration" (show $ duration event.eventTime)
      , Tuple "EndTime" (show $ endTime event.eventTime)
      ]

assert :: forall a b. String -> Either a b -> Aff Unit
assert x (Left _)  = liftEffect $ Exception.throw x
assert _ (Right _)   = pure unit 

insertFlowURI :: Flow.Event -> Aff String
insertFlowURI (Flow.Event event) = do
  _ <- assert "Parsing/validation failure (Flow)." $ runParser (show $ Flow.Event event) Flow.event
  pure $ insertURI' "Flow" params
  where
    params = 
      [ Tuple "SIP" (show event.sIP)
      , Tuple "DIP" (show event.dIP)
      , Tuple "SPort" (show event.sPort)
      , Tuple "DPort" (show event.dPort)
      , Tuple "Protocol" (show event.protocol)
      , Tuple "Packets" (show event.packets)
      , Tuple "Bytes" (show event.bytes)
      , Tuple "Flags" (intercalate "" (show <$> event.flags))
      , Tuple "StartTIme" (show event.startTime)
      , Tuple "Duration" (show event.duration)
      , Tuple "EndTime" (show event.endTime)
      ]

insertWindowsURI :: Windows.Event -> Aff String
insertWindowsURI (Windows.Event event) = do
  pure $ insertURI' "Windows" params
  where 
    params  =
      [ Tuple "EventCategory" (show event.eventCategory)
      , Tuple "EventType" (show event.eventType)
      , Tuple "EventID" (show event.eventID)
      , Tuple "EventURI" (show event.eventURI)
      , Tuple "EventSource" (show event.eventSource)
      , Tuple "StartTime" $ case event.eventTime of (Event.Time x) -> show x.startTime
      , Tuple "Duration"  $ case event.eventTime of (Event.Time x) -> show x.duration
      , Tuple "EndTime"   $ case event.eventTime of (Event.Time x) -> show x.endTime
      ]

insertStatisticsURI :: Statistics.Event -> Aff String
insertStatisticsURI (Statistics.Event event) = do
  pure $ insertURI' "Statistics" params
  where 
    params  = 
      case event.eventURI of
        (Statistics.EventURI eventURI) ->
          [ Tuple "EventType" (show event.eventType)
          , Tuple "EventCategory" (show event.eventCategory)
          , Tuple "EventID" (show event.eventID)
          , Tuple "Min" (show eventURI.min)
          , Tuple "Max" (show eventURI.max)
          , Tuple "Sum" (show eventURI.sum)
          , Tuple "Total" (show eventURI.total)
          , Tuple "Average" (show eventURI.average)
          , Tuple "Variance" (show eventURI.variance)
          ]

insertURI' ::  Table -> Array (Tuple String String) -> String
insertURI'  table' params = query
  where
     query = "INSERT INTO " <> table' <> " (" <> columns <> ") VALUES (" <> values <> ")"
     columns  = "'" <> (intercalate "','" columns') <> "'"
     values   = "'" <> (intercalate "','" values') <> "'"
     columns' = fst <$> params
     values'  = snd <$> params

insertURI :: Forward.URI ->  Aff String
insertURI (Forward.Audit event)      = insertAuditURI event
insertURI (Forward.Flow  event)      = insertFlowURI event
insertURI (Forward.Statistics event) = insertStatisticsURI event
insertURI (Forward.Windows event)    = insertWindowsURI event

reportAuditURI' :: Report.ReportType -> Table -> Table
reportAuditURI' Report.Source   = \table -> "SELECT COUNT(*) AS X FROM (" <> table <> ") GROUP BY EventSource" 
reportAuditURI' Report.Time = \table -> "SELECT Duration as X FROM (" <> table <> ")"

reportURI :: Report.URI -> Table
reportURI (Report.Audit eventCategory eventType eventID reportType) = reportAuditURI' reportType $ "SELECT * FROM Audit WHERE EventCategory='" <> show eventCategory <> "' AND EventType='" <> show eventType <> "' AND EventID='" <> show eventID <> "'"

maxURI :: Report.URI -> String
maxURI report = "SELECT MAX(X) AS Y FROM (" <> (reportURI report) <> ")"

minURI :: Report.URI -> String
minURI report = "SELECT MIN(X) AS Y FROM (" <> (reportURI report) <> ")"

averageURI :: Report.URI -> String
averageURI report = "SELECT AVG(X) AS Y FROM (" <> (reportURI report) <> ")"

sumURI :: Report.URI -> String
sumURI report = "SELECT SUM(X) AS Y FROM (" <> (reportURI report) <> ")"

totalURI :: Report.URI -> String
totalURI report = "SELECT COUNT(*) as Y FROM (" <> (reportURI report) <> ")"

varianceURI :: Report.URI -> Number -> String
varianceURI report avg = query
  where 
    query  = "SELECT AVG(" <> query' <> " * " <> query' <> ") AS Y FROM (" <> (reportURI report) <> ")"
    query' = "(X - " <> show avg <> ")"

interpret :: forall a. Query (Request a) -> Aff (Request a)
interpret (DSL.Forward (Settings _ _ dbms) query next) = do
  result <- executeForward dbms query
  next <$> pure result
interpret (DSL.Report (Settings _ _ dbms) query next) = do
  result <- executeReport dbms query
  next <$> (pure result)

executeTouch :: String -> Aff Unit
executeTouch file = do
  database <- SQLite3.connect file SQLite3.OpenCreate
  _        <- SQLite3.close database
  pure unit

executeSchemas :: String -> Aff Unit
executeSchemas file = do
  database <- SQLite3.connect file SQLite3.OpenReadWrite
  _        <- SQLite3.all (schemaURI Schema.Audit) database
  _        <- SQLite3.all (schemaURI Schema.Flow) database
  _        <- SQLite3.all (schemaURI Schema.Statistics) database
  _        <- SQLite3.all (schemaURI Schema.Windows) database
  _        <- SQLite3.close database
  pure unit

executeForward :: Target -> Forward.URI -> Aff Resource
executeForward (Failover uri) query = do
  result  <- try $ executeForward (Replication uri) query
  result' <- try $ Aff.sequential (Foldable.oneOf (Aff.parallel <$> flip executeForward query <$> failoverSites uri))
  case result of
    (Left _) -> case result' of
      (Left _)          -> liftEffect $ Exception.throw $ "Failover failure (" <> show query <> ")" 
      (Right resource') -> pure resource'
    (Right resource') -> pure $ resource'
executeForward (Replication uri) query  = do
  _ <- Aff.sequential (sequence (Aff.parallel <$> flip executeForward query <$> replicationSites uri)) 
  pure (Forward unit)
executeForward (Single uri) query = do
  _        <- executeTouch (path uri)
  _        <- executeSchemas (path uri)
  database <- SQLite3.connect (path uri) SQLite3.OpenReadWrite
  uri'     <- insertURI query
  _        <- SQLite3.all uri' database
  _        <- SQLite3.close database
  pure (Forward unit)

path :: URI -> String
path (Primary Production)   = "production.primary.db"
path (Secondary Production) = "production.secondary.db"
path (Offsite Production)   = "production.offsite.db"
path (Primary Testing)      = "testing.primary.db"
path (Secondary Testing)    = "testing.secondary.db" 
path (Offsite Testing)      = "testing.offsite.db"

failoverSites :: URI -> Array Target
failoverSites (Primary Production)   = [Single (Offsite Production)]
failoverSites (Secondary Production) = [Single (Offsite Production)]
failoverSites (Offsite Production)   = []
failoverSites (Primary Testing)      = [Single (Offsite Testing)]
failoverSites (Secondary Testing)    = [Single (Offsite Testing)]
failoverSites (Offsite Testing)      = []

replicationSites :: URI -> Array Target
replicationSites (Primary Production)   = [Single (Primary Production), Single (Secondary Production)]
replicationSites (Secondary Production) = [Single (Primary Production), Single (Secondary Production)]
replicationSites (Offsite Production)   = [Single (Offsite Production)]
replicationSites (Primary Testing)      = [Single (Primary Testing), Single (Secondary Testing)]
replicationSites (Secondary Testing)    = [Single (Primary Testing), Single (Secondary Testing)]
replicationSites (Offsite Testing)      = [Single (Offsite Testing)]

{-- | Combines replicated resources if they are equivalent. --}
replication :: Array Resource -> Maybe Resource
replication w = foldl f (Array.head w) (sequence $ Array.tail w)
  where
    f :: Maybe Resource -> Maybe Resource -> Maybe Resource
    f (Just (Forward x)) (Just (Forward y)) = if (x == y) then (Just (Forward x)) else (Nothing)
    f (Just (Report x)) (Just (Report y))   = if (x == y) then (Just (Report x))  else (Nothing)
    f  _                _                   = Nothing

executeReport :: Target -> Report.URI -> Aff Resource
executeReport (Failover uri) query = do
  result  <- try $ executeReport (Replication uri) query
  case result of
    (Left _) -> do
      result' <- try $ Aff.sequential (Foldable.oneOf (Aff.parallel <$> flip executeReport query <$> failoverSites uri))
      case result' of
        (Left _)          -> liftEffect $ Exception.throw "Failover failure."
        (Right resource') -> pure resource'
    (Right resource') -> pure $ resource'
executeReport (Replication uri) query = do
  events <- Aff.sequential (sequence (Aff.parallel <$> flip executeReport query <$> replicationSites uri))
  result <- pure $ replication events
  case result of
    (Just event) -> pure event
    (Nothing)    -> liftEffect $ Exception.throw "Replication failure."
executeReport (Single uri) query = do
  _          <- executeTouch (path uri)
  _          <- executeSchemas (path uri)
  database   <- SQLite3.connect (path uri) SQLite3.OpenReadOnly
  min        <- executeReport'' database (minURI query)
  max        <- executeReport'' database (maxURI query)
  sum        <- executeReport'' database (sumURI query)
  total      <- executeReport'' database (totalURI query)
  average    <- executeReport'' database (averageURI query)
  variance   <- executeReport'' database (varianceURI query average)
  _          <- SQLite3.close database
  {-- todo: derive event time from authorization settings --}
  eventTime  <- pure $ Event.Time { startTime : Date.epoch, duration : 0, endTime : Date.epoch }
  eventURI   <- pure $ Statistics.EventURI $
    { min           : Math.floor min
    , max           : Math.floor max
    , sum           : Math.floor sum
    , total         : Math.floor total
    , average       : Math.floor average
    , variance      : Math.floor variance
    }
  pure $ Report $ Statistics.Event $
    { eventCategory : eventCategory query
    , eventType     : eventType query
    , eventID       : eventID query
    , eventTime     : eventTime
    , eventSource   : Event.Tier3
    , eventURI      : eventURI
    }
  where
    eventID       (Report.Audit _ _ _ _)                = Statistics.Audit
    eventType     _                                     = Statistics.Success
    eventCategory (Report.Audit _ _ _ (Report.Source))  = Statistics.Source
    eventCategory (Report.Audit _ _ _ (Report.Time))    = Statistics.Duration

executeReport'' :: Connection -> String -> Aff Number
executeReport'' database uri = do
  rows    <- SQLite3.all uri database
  results <- sequence (runResult <$> rows)
  case results of
    [number'] -> pure number'
    _         -> throwError error
  where
    runResult row = do
       result' <- pure (runExcept $ row ! "Y" >>= Foreign.readNumber)
       case result' of
         (Left _)        -> pure 0.0
         (Right number') -> pure number'
    error = Exception.error "Unexpected results."

resource :: Settings -> Route -> Request Resource
resource settings (Route.Forward query) = liftFreeT $ (DSL.Forward settings query identity)
resource settings (Route.Report query)  = liftFreeT $ (DSL.Report settings query identity)

audit :: Settings -> Route -> Request Resource
audit = \settings route -> do
  startTime'  <- lift $ liftEffect $ Date.current
  result      <- lift $ execute (resource settings route)
  endTime'    <- lift $ liftEffect $ Date.current
  duration'   <- pure $ Math.floor $ (Date.getTime endTime') - (Date.getTime startTime')
  eventCategory <- pure $ case route of
    (Route.Forward _) -> Audit.Forward
    (Route.Report  _) -> Audit.Report
  eventID     <- pure $ case route of
    (Route.Forward (Forward.Audit _))      -> Audit.Audit
    (Route.Forward (Forward.Flow _))       -> Audit.Traffic
    (Route.Forward (Forward.Statistics _)) -> Audit.Statistics
    (Route.Forward (Forward.Windows _))    -> Audit.Windows
    (Route.Report (Report.Audit _ _ _ _))  -> Audit.Audit
  eventType   <- pure $ case result of
    (Left _)  -> Audit.Failure
    (Right _) -> Audit.Success
  eventTime   <- pure $ Event.Time { startTime : startTime', duration : duration', endTime : endTime' }
  {-- todo: derive event source from auth. settings --}
  eventSource <- pure $ Event.Tier3
  {-- todo: derive namespace UUID from auth. settings --}
  sourceUUID  <- lift $ liftEffect $ UUID.uuidv1
  eventURI    <- lift $ liftEffect $ UUID.uuidv5 (show route) sourceUUID
  event       <- pure $ Audit.Event $
                 { eventCategory : eventCategory
                 , eventType     : eventType
                 , eventID       : eventID
                 , eventSource   : eventSource
                 , eventURI      : eventURI
                 , eventTime     : eventTime
                 }
  _           <- lift $ execute (resource settings $ Route.Forward (Forward.Audit event))
  case result of
    (Left _)  -> lift $ liftEffect $ Exception.throw (show route)
    (Right x) -> pure x

request :: Settings -> Route -> Request Resource
request = audit

execute ::  forall a. Request a -> Aff (Result a)
execute = try <<< runFreeT interpret
