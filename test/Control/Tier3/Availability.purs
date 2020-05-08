module Test.Control.Tier3.Availability 
  ( suite
  ) where

import Prelude

import Control.Monad.Trans.Class (lift)

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Traversable (sequence)

import Effect.Aff (Aff)
import Effect.Console (log) as Console
import Effect.Class (liftEffect)
import Effect.Exception (throw) as Exception

import Effect.Audit (random) as Audit
import Effect.Flow (random) as Flow
import Effect.Linux (random) as Linux
import Effect.Statistics (random) as Statistics
import Effect.Windows (random) as Windows

import FFI.Date as Date
import FFI.FS as FS
import FFI.Math as Math

import Data.Audit (EventCategory(..), EventType(..), EventID(..)) as Audit
import Data.Statistics (Event(..), EventURI(..)) as Statistics

import Data.Event (Time(..)) as Event

import Control.Authorization as Authorization
import Control.Authentication as Authentication

import Data.Forward as Forward
import Data.Report (URI(..), ReportType(..), uris) as Report
import Data.Route as Route

import Control.Tier3 as Tier3

import Test.Data.Test as Test

min' :: Statistics.Event -> Int
min' (Statistics.Event x) =
  case x.eventURI of
    (Statistics.EventURI y) -> y.min

max' :: Statistics.Event -> Int
max' (Statistics.Event x) =
  case x.eventURI of
    (Statistics.EventURI y) -> y.max

sum :: Statistics.Event -> Int
sum (Statistics.Event x) =
  case x.eventURI of
    (Statistics.EventURI y) -> y.sum

total :: Statistics.Event -> Int
total (Statistics.Event x) =
  case x.eventURI of
    (Statistics.EventURI y) -> y.total

average :: Statistics.Event -> Int
average (Statistics.Event x) =
  case x.eventURI of
    (Statistics.EventURI y) -> y.average

variance :: Statistics.Event -> Int
variance (Statistics.Event x) =
  case x.eventURI of
    (Statistics.EventURI y) -> y.variance

report :: Tier3.Settings -> Report.URI -> Tier3.Request Unit
report settings uri = do
  x <- Tier3.request settings (Route.Report uri)
  case x of
    (Tier3.Forward _) -> lift $ liftEffect $ Exception.throw "Unexpected behaviour." 
    (Tier3.Report y)  -> do
       _ <- pure $ foldl (&&) true (flip (>=) 0 <$> [min' y, max' y, sum y, total y, average y, variance y])
       pure unit

reports :: Tier3.Settings -> Tier3.Request Unit
reports settings = do
  _ <- failures settings
  _ <- sequence (report settings <$> Report.uris) 
  _ <- failures settings
  pure unit

forwardAudit :: Tier3.Settings -> Tier3.Request Unit
forwardAudit settings  = do
  event <- lift $ liftEffect Audit.random
  _     <- Tier3.request settings (Route.Forward (Forward.Audit event)) 
  pure unit 

forwardFlow :: Tier3.Settings -> Tier3.Request Unit
forwardFlow settings = do
  event <- lift $ liftEffect Flow.random
  _     <- Tier3.request settings (Route.Forward (Forward.Flow event)) 
  pure unit 

forwardLinux :: Tier3.Settings -> Tier3.Request Unit
forwardLinux settings = do
  event <- lift $ liftEffect Linux.random
  _     <- Tier3.request settings (Route.Forward (Forward.Linux event)) 
  pure unit 

forwardReport :: Tier3.Settings -> Tier3.Request Unit
forwardReport settings = do
  event <- lift $ liftEffect Statistics.random
  _     <- Tier3.request settings (Route.Forward (Forward.Statistics event)) 
  pure unit 

forwardWindows :: Tier3.Settings -> Tier3.Request Unit
forwardWindows settings = do
  event <- lift $ liftEffect Windows.random
  _     <- Tier3.request settings (Route.Forward (Forward.Windows event)) 
  pure unit 
   
forward :: Tier3.Settings -> Tier3.Request Unit
forward settings = do
  choice <- lift $ liftEffect (Math.floor <$> ((*) 5.0) <$> Math.random)
  case choice of
    0 -> forwardAudit   settings
    1 -> forwardFlow    settings
    2 -> forwardReport  settings
    3 -> forwardLinux   settings
    _ -> forwardWindows settings 

forwards :: Tier3.Settings -> Int -> Tier3.Request Unit
forwards = \settings n ->  do
  _ <- failures settings
  _ <- sequence ((const (successes settings $ forward settings)) <$> Array.range 1 n) 
  _ <- failures settings
  pure unit

failure :: Tier3.Settings -> Report.ReportType -> Audit.EventCategory -> Audit.EventID -> Tier3.Request Unit
failure settings reportType eventCategory eventID = do
  x <- Tier3.request settings (Route.Report (Report.Audit eventCategory Audit.Failure eventID reportType))
  case x of
    (Tier3.Forward _) -> lift (liftEffect $ Exception.throw "Unexpected behaviour.")
    (Tier3.Report y)  -> do
      result <- pure $ foldl (&&) true (flip (==) 0 <$> [min' y, max' y, sum y, total y, average y, variance y])
      case result of
        true  -> pure unit 
        _    -> lift (liftEffect $ Exception.throw "Unexpected audited failure event format.")

failures :: Tier3.Settings -> Tier3.Request Unit
failures settings = void $ sequence $
  [ failure settings Report.Source Audit.Forward Audit.Traffic
  , failure settings Report.Source Audit.Forward Audit.Statistics
  , failure settings Report.Source Audit.Forward Audit.Linux
  , failure settings Report.Source Audit.Forward Audit.Windows
  , failure settings Report.Time   Audit.Forward Audit.Traffic
  , failure settings Report.Time   Audit.Forward Audit.Statistics
  , failure settings Report.Time   Audit.Forward Audit.Linux 
  , failure settings Report.Time   Audit.Forward Audit.Windows
  ]

success :: forall a. Tier3.Settings -> Report.ReportType -> Audit.EventCategory -> Audit.EventID -> Tier3.Request a -> Tier3.Request Unit
success settings reportType eventCategory eventID = \request -> do
  w <- Tier3.request settings (Route.Report (Report.Audit eventCategory Audit.Success eventID reportType))
  _ <- request
  x <- Tier3.request settings (Route.Report (Report.Audit eventCategory Audit.Success eventID reportType))
  case [w,x] of
    [(Tier3.Report y), (Tier3.Report z)]  -> do
      result <- pure $ foldl (&&) true [min' z >= 0, max' z >= max' y, sum z >= sum y, total z >= total y, average z >= 0, variance z >= 0]
      case result of
        true  -> pure unit
        false -> lift (liftEffect $ Exception.throw "Comparison of audited success events failed.")
    _ -> lift (liftEffect $ Exception.throw "Unexpected result set type.")

successes :: forall a. Tier3.Settings -> Tier3.Request a -> Tier3.Request Unit
successes settings request = void $ sequence $ apply request <$>
  [ success settings Report.Source Audit.Forward Audit.Traffic
  , success settings Report.Source Audit.Forward Audit.Statistics
  , success settings Report.Source Audit.Forward Audit.Linux
  , success settings Report.Source Audit.Forward Audit.Windows
  , success settings Report.Time   Audit.Forward Audit.Traffic
  , success settings Report.Time   Audit.Forward Audit.Statistics
  , success settings Report.Time   Audit.Forward Audit.Linux 
  , success settings Report.Time   Audit.Forward Audit.Windows
  ]
  where apply x = \f -> f x

unlink :: Tier3.URI -> Tier3.Request Unit
unlink (Tier3.Primary Tier3.Testing)   = lift $ void $ FS.unlink "testing.primary.db"
unlink (Tier3.Secondary Tier3.Testing) = lift $ void $ FS.unlink "testing.secondary.db"
unlink (Tier3.Offsite Tier3.Testing)   = lift $ void $ FS.unlink "testing.offsite.db"
unlink _                               = pure unit

fresh :: Tier3.Request Unit
fresh = unlink (Tier3.Primary Tier3.Testing) *> unlink (Tier3.Secondary Tier3.Testing) *> unlink (Tier3.Offsite Tier3.Testing)

testSingleForward :: Tier3.Request Unit
testSingleForward = do
  _         <- fresh
  startTime <- lift $ liftEffect $ Date.current
  _         <- forwards settings n
  endTime   <- lift $ liftEffect $ Date.current
  _         <- fresh
  duration  <- pure $ Math.floor ((Date.getTime endTime) - (Date.getTime startTime))
  event     <- pure $ Test.Event $
    { eventCategory : Test.Tier3
    , eventType     : Test.Single
    , eventID       : Test.Forward
    , eventURI      : unit
    , eventTime     : Event.Time $ { startTime : startTime, duration : duration, endTime : endTime }
    }
  _     <- lift $ liftEffect (Console.log $ show event)
  pure unit
  where 
    settings = Tier3.Settings Authorization.Default Authentication.Default dbms
    dbms     = Tier3.Single $ Tier3.Primary Tier3.Testing
    n        = 10

testReplicationForward :: Tier3.Request Unit
testReplicationForward = do
  _         <- fresh
  startTime <- lift $ liftEffect $ Date.current
  _         <- forwards settings n
  endTime   <- lift $ liftEffect $ Date.current
  _         <- fresh
  duration  <- pure $ Math.floor ((Date.getTime endTime) - (Date.getTime startTime))
  event     <- pure $ Test.Event $
    { eventCategory : Test.Tier3
    , eventType     : Test.Replication
    , eventID       : Test.Forward
    , eventURI      : unit
    , eventTime     : Event.Time $ { startTime : startTime, duration : duration, endTime : endTime }
    }
  _     <- lift $ liftEffect (Console.log $ show event)
  pure unit
  where 
    settings = Tier3.Settings Authorization.Default Authentication.Default dbms
    dbms     = Tier3.Replication $ Tier3.Primary Tier3.Testing
    n        = 1

testSingleReports :: Tier3.Request Unit
testSingleReports = do
  _         <- fresh
  startTime <- lift $ liftEffect $ Date.current
  _         <- reports settings
  endTime   <- lift $ liftEffect $ Date.current
  _         <- fresh
  duration  <- pure $ Math.floor ((Date.getTime endTime) - (Date.getTime startTime))
  event     <- pure $ Test.Event $
    { eventCategory : Test.Tier3
    , eventType     : Test.Single
    , eventID       : Test.Report
    , eventURI      : unit
    , eventTime     : Event.Time $ { startTime : startTime, duration : duration, endTime : endTime }
    }
  _     <- lift $ liftEffect (Console.log $ show event)
  pure unit
  where 
    settings = Tier3.Settings Authorization.Default Authentication.Default dbms
    dbms     = Tier3.Single $ Tier3.Primary Tier3.Testing

testReplicationReports :: Tier3.Request Unit
testReplicationReports = do
  _         <- fresh
  startTime <- lift $ liftEffect $ Date.current
  _         <- reports settings
  endTime   <- lift $ liftEffect $ Date.current
  _         <- fresh
  duration  <- pure $ Math.floor ((Date.getTime endTime) - (Date.getTime startTime))
  event     <- pure $ Test.Event $
    { eventCategory : Test.Tier3
    , eventType     : Test.Replication
    , eventID       : Test.Report
    , eventURI      : unit
    , eventTime     : Event.Time $ { startTime : startTime, duration : duration, endTime : endTime }
    }
  _     <- lift $ liftEffect (Console.log $ show event)
  pure unit
  where 
    settings = Tier3.Settings Authorization.Default Authentication.Default dbms
    dbms     = Tier3.Replication $ Tier3.Primary Tier3.Testing

testFailoverForwards :: Tier3.Request Unit
testFailoverForwards = do
  _         <- fresh
  startTime <- lift $ liftEffect $ Date.current
  _         <- forwards settings n
  endTime   <- lift $ liftEffect $ Date.current
  _         <- fresh
  duration  <- pure $ Math.floor ((Date.getTime endTime) - (Date.getTime startTime))
  event     <- pure $ Test.Event $
    { eventCategory : Test.Tier3
    , eventType     : Test.Failover
    , eventID       : Test.Forward
    , eventURI      : unit
    , eventTime     : Event.Time $ { startTime : startTime, duration : duration, endTime : endTime }
    }
  _     <- lift $ liftEffect (Console.log $ show event)
  pure unit
  where 
    settings = Tier3.Settings Authorization.Default Authentication.Default dbms
    dbms     = Tier3.Failover $ Tier3.Primary Tier3.Testing
    n        = 1

testFailoverReports :: Tier3.Request Unit
testFailoverReports = do
  _         <- fresh
  startTime <- lift $ liftEffect $ Date.current
  _         <- reports settings
  endTime   <- lift $ liftEffect $ Date.current
  _         <- fresh
  duration  <- pure $ Math.floor ((Date.getTime endTime) - (Date.getTime startTime))
  event     <- pure $ Test.Event $
    { eventCategory : Test.Tier3
    , eventType     : Test.Failover
    , eventID       : Test.Report
    , eventURI      : unit
    , eventTime     : Event.Time $ { startTime : startTime, duration : duration, endTime : endTime }
    }
  _     <- lift $ liftEffect (Console.log $ show event)
  pure unit
  where 
    settings = Tier3.Settings Authorization.Default Authentication.Default dbms
    dbms     = Tier3.Failover $ Tier3.Primary Tier3.Testing

tests :: Tier3.Request Unit
tests = void $ sequence $
  [ testSingleForward
  , testSingleReports
  , testReplicationForward
  , testReplicationReports
  , testFailoverForwards
  , testFailoverReports
  ]

suite :: Aff Unit
suite =  do
  x <- Tier3.execute tests
  case x of
    (Left _)  -> do
       _ <- liftEffect $ Exception.throw "Data-access tier availability tests failed."
       pure unit
    (Right _) -> do
       pure unit
  pure unit
