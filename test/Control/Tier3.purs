module Test.Control.Tier3 
  ( suite
  , main
  ) where

import Prelude

import Control.Monad.Trans.Class (lift)

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl, intercalate)
import Data.Traversable (sequence)

import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Console (log) as Console
import Effect.Class (liftEffect)
import Effect.Exception (throw) as Exception

import Effect.Audit (random) as Audit
import Effect.Alert (random) as Alert
import Effect.Flow (random) as Flow
import Effect.Linux (random) as Linux
import Effect.Report (random) as Report
import Effect.Windows (random) as Windows

import FFI.Math as Math

import Data.IPv4 (IPv4(..))

import Data.Audit (EventCategory(..), EventType(..), EventID(..), ReportType(..)) as Audit
import Data.Report (Event(..)) as Report
import Data.Schema as Schema

import Data.Tier3.Forward as Forward
import Data.Tier3.Report (URI(..), uris) as Report
import Data.Tier3.Route as Route

import Control.Tier3 as Tier3

report :: Tier3.Settings -> Report.URI -> Tier3.Request Unit
report settings uri = do
  x <- Tier3.request settings (Route.Report uri)
  case x of
    (Tier3.Forward _)               -> lift $ liftEffect $ Exception.throw "Unexpected behaviour." 
    (Tier3.Report (Report.Event y)) -> do
       _ <- pure $ foldl (&&) true (flip (>=) 0 <$> [y.min, y.max, y.sum, y.total, y.average, y.variance])
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

forwardAlert :: Tier3.Settings -> Tier3.Request Unit
forwardAlert settings =  do
  event <- lift $ liftEffect Alert.random
  _     <- Tier3.request settings (Route.Forward (Forward.Alert event)) 
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
  event <- lift $ liftEffect Report.random
  _     <- Tier3.request settings (Route.Forward (Forward.Report event)) 
  pure unit 

forwardWindows :: Tier3.Settings -> Tier3.Request Unit
forwardWindows settings = do
  event <- lift $ liftEffect Windows.random
  _     <- Tier3.request settings (Route.Forward (Forward.Windows event)) 
  pure unit 
   
forward :: Tier3.Settings -> Tier3.Request Unit
forward settings = do
  choice <- lift $ liftEffect (Math.floor <$> ((*) 6.0) <$> Math.random)
  case choice of
    0 -> forwardAudit   settings
    1 -> forwardAlert   settings
    2 -> forwardFlow    settings
    3 -> forwardReport  settings
    4 -> forwardLinux   settings
    _ -> forwardWindows settings 

forwards :: Tier3.Settings -> Int -> Tier3.Request Unit
forwards = \settings n ->  do
  _ <- failures settings
  _ <- sequence ((const (successes settings $ forward settings)) <$> Array.range 1 n) 
  _ <- failures settings
  pure unit

failure :: Tier3.Settings -> Audit.ReportType -> Audit.EventID -> Tier3.Request Unit
failure settings reportType eventID = do
  x <- Tier3.request settings (Route.Report (Report.Audit Audit.Tier3 Audit.Failure eventID reportType))
  case x of
    (Tier3.Forward _)                -> lift (liftEffect $ Exception.throw "Unexpected behaviour.")
    (Tier3.Report (Report.Event y))  -> do
      result <- pure $ foldl (&&) true (flip (==) 0 <$> [y.min, y.max, y.sum, y.total, y.average, y.variance])
      case result of
        true  -> pure unit 
        _     -> lift (liftEffect $ Exception.throw "Unexpected behaviour.")

failures :: Tier3.Settings -> Tier3.Request Unit
failures settings = void $ sequence $
  [ failure settings Audit.Source $ Audit.Forward Schema.Audit
  , failure settings Audit.Source $ Audit.Forward Schema.Alert
  , failure settings Audit.Source $ Audit.Forward Schema.Flow
  , failure settings Audit.Source $ Audit.Forward Schema.Report
  , failure settings Audit.Source $ Audit.Forward Schema.Linux
  , failure settings Audit.Source $ Audit.Forward Schema.Windows
  , failure settings Audit.Duration $ Audit.Forward Schema.Audit
  , failure settings Audit.Duration $ Audit.Forward Schema.Alert
  , failure settings Audit.Duration $ Audit.Forward Schema.Flow
  , failure settings Audit.Duration $ Audit.Forward Schema.Report
  , failure settings Audit.Duration $ Audit.Forward Schema.Linux
  , failure settings Audit.Duration $ Audit.Forward Schema.Windows
  ]

success :: forall a. Tier3.Settings -> Audit.ReportType -> Audit.EventID -> Tier3.Request a -> Tier3.Request Unit
success settings reportType eventID = \request -> do
  w <- Tier3.request settings (Route.Report (Report.Audit Audit.Tier3 Audit.Success eventID reportType))
  _ <- request
  x <- Tier3.request settings (Route.Report (Report.Audit Audit.Tier3 Audit.Success eventID reportType))
  case [w,x] of
    [(Tier3.Report (Report.Event y)), (Tier3.Report (Report.Event z))]  -> do
      result <- pure $ case settings of
                  (Tier3.Settings _ _ (Tier3.Local _)) -> foldl (&&) true [y.min >= 0, z.max >= y.max, z.sum >= y.sum, z.total >= y.total, y.average >= 0, y.variance >= 0]
                  _                                    -> foldl (&&) true [y.min >= 0, z.max >= y.max, z.sum >= y.sum, z.total >= y.total, y.average >= 0, y.variance >= 0]
--      result <- pure $ foldl (&&) true ([z.min <= y.min, z.max >= y.max, z.sum > y.sum, z.total <= (y.total + 1), z.average >= 0, z.variance >= 0])
      case result of
        true  -> pure unit
        false -> lift (liftEffect $ Exception.throw "Unexpected behaviour.")
    _         -> lift (liftEffect $ Exception.throw "Unexpected behaviour.")

successes :: forall a. Tier3.Settings -> Tier3.Request a -> Tier3.Request Unit
successes settings request = void $ sequence $ apply request <$>
  [ success settings Audit.Source $ Audit.Forward Schema.Alert
  , success settings Audit.Source $ Audit.Forward Schema.Flow
  , success settings Audit.Source $ Audit.Forward Schema.Report
  , success settings Audit.Source $ Audit.Forward Schema.Linux
  , success settings Audit.Source $ Audit.Forward Schema.Windows
  , success settings Audit.Duration $ Audit.Forward Schema.Alert
  , success settings Audit.Duration $ Audit.Forward Schema.Flow
  , success settings Audit.Duration $ Audit.Forward Schema.Report
  , success settings Audit.Duration $ Audit.Forward Schema.Linux 
  , success settings Audit.Duration $ Audit.Forward Schema.Windows
  ]
  where apply x = \f -> f x

requests :: Tier3.Request Unit
requests = void $ sequence $
  [ test ["[RUNNING]", "RELEASE-01/TIER-03", "LOCAL/FORWARD"]       *> forwards (settings $ local 0 0) 100
  , test ["[RUNNING]", "RELEASE-01/TIER-03", "REPLICATION/FORWARD"] *> forwards (settings $ replication 0 1) 100
  , test ["[RUNNING]", "RELEASE-01/TIER-03", "LOCAL/REPORT"]        *> reports  (settings $ local 1 0)
  , test ["[RUNNING]", "RELEASE-01/TIER-03", "REPLICATION/REPORT"]  *> reports (settings $ replication 1 1)
  ]
  where
    test x          = lift $ liftEffect $ Console.log $ intercalate " " x
    settings x      = Tier3.Settings (Tier3.Authorization unit) (Tier3.Authentication origin) x
    local n m       = Tier3.Local $ "Test.Control.Tier3." <> show n <> "." <> show m <> ".db"
    replication n m = Tier3.Replication $ local n <$> Array.range 0 1
    origin          = { sIP : IPv4 0 0 0 0, sPort : 0 }

suite :: Aff Unit
suite =  do
  _ <- liftEffect $ Console.log $ intercalate " " ["[RUNNING]", "RELEASE-01/TIER-03", "*", "*"]
  x <- Tier3.execute requests
  case x of
    (Left _)  -> do
       _ <- liftEffect $ Console.log     $ intercalate " " ["[FAILURE]", "RELEASE-01/TIER-03", "*", "*"]
       _ <- liftEffect $ Exception.throw $ intercalate " " ["[FAILURE]", "RELEASE-01/TIER-03", "*", "*"]
       pure unit
    (Right _) -> do
       _ <- liftEffect $ Console.log $ intercalate " " ["[SUCCESS]", "RELEASE-01/TIER-03", "*", "*"]
       pure unit
  pure unit

main :: Effect Unit
main = void $ launchAff $ suite
