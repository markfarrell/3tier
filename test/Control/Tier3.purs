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

import Data.Report (Event(..)) as Report

import Data.Tier3.Forward as Forward
import Data.Tier3.Report (URI, uris) as Report
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
reports settings = void $ sequence (report settings <$> Report.uris) 

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

forwards :: Tier3.Settings -> Tier3.Request Unit
forwards settings = void $ sequence (const (forward settings) <$> Array.range 1 100) 

requests :: Tier3.Request Unit
requests = void $ sequence $
  [ test ["[RUNNING]", "RELEASE-01/TIER-03", "LOCAL", "FORWARD"]       *> forwards (settings $ local 0 0)
  , test ["[RUNNING]", "RELEASE-01/TIER-03", "LOCAL", "REPORT"]        *> reports  (settings $ local 1 0)
  , test ["[RUNNING]", "RELEASE-01/TIER-03", "REPLICATION", "FORWARD"] *> forwards (settings $ replication 0 1)
  , test ["[RUNNING]", "RELEASE-01/TIER-03", "REPLICATION", "REPORT"]  *> reports (settings $ replication 1 1)
  ]
  where
    test x          = lift $ liftEffect $ Console.log $ intercalate " " x
    settings x      = Tier3.Settings (Tier3.Authorization unit) (Tier3.Authentication unit) x
    local n m       = Tier3.Local $ "Test.Control.Tier3." <> show n <> "." <> show m <> ".db"
    replication n m = Tier3.Replication $ local n <$> Array.range 0 m

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
