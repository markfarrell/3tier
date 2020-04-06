module Test.Tier3 where

import Prelude

import Control.Monad.Trans.Class (lift)

import Data.Array as Array
import Data.Traversable (sequence)

import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)

import Effect.Audit (random) as Audit
import Effect.Alert (random) as Alert
import Effect.Flow (random) as Flow
import Effect.Linux (random) as Linux
import Effect.Report (random) as Report
import Effect.Windows (random) as Windows

import FFI.Math as Math

import Tier.Forward as Forward
import Tier.Report (all) as Report
import Tier.Route as Route

import Tier3 as Tier3

nothing :: Tier3.Request Unit
nothing = pure unit

replication :: Int -> Tier3.Request Unit
replication n = do
  _ <- sequence $ Tier3.request settings <$> routes
  pure unit
  where 
    settings       = Tier3.Settings authorization authentication replicated
    replicated     = Tier3.Replication $ dbms <$> range n
    dbms  m        = Tier3.Local $ "Test.Tier3.replication.db." <> show m
    routes         = Route.Report <$> Report.all
    range m        = case m > 0 of
                       true  -> Array.range 1 m
                       false -> []
    authorization  = Tier3.Authorization unit
    authentication = Tier3.Authentication unit

fail :: Tier3.Request Unit
fail = do
  _ <- nothing
  _ <- replication 0
  _ <- replication 1
  pure unit

many :: Tier3.Request Unit
many = do
  _ <- replication 1
  _ <- replication 10
  _ <- replication 100
  pure unit

forwardAudit :: Tier3.Request Unit
forwardAudit = do
  event <- lift $ liftEffect Audit.random
  _     <- Tier3.request settings (Route.Forward (Forward.Audit event)) 
  pure unit 
  where
    settings = Tier3.Settings authorization authentication dbms
    dbms     = Tier3.Local $ "Test.Tier3.forwards.db"
    authorization  = Tier3.Authorization unit
    authentication = Tier3.Authentication unit

forwardAlert :: Tier3.Request Unit
forwardAlert = do
  event <- lift $ liftEffect Alert.random
  _     <- Tier3.request settings (Route.Forward (Forward.Alert event)) 
  pure unit 
  where
    settings = Tier3.Settings authorization authentication dbms
    dbms     = Tier3.Local $ "Test.Tier3.forwards.db"
    authorization  = Tier3.Authorization unit
    authentication = Tier3.Authentication unit


forwardFlow :: Tier3.Request Unit
forwardFlow = do
  event <- lift $ liftEffect Flow.random
  _     <- Tier3.request settings (Route.Forward (Forward.Flow event)) 
  pure unit 
  where
    settings = Tier3.Settings authorization authentication dbms
    dbms     = Tier3.Local $ "Test.Tier3.forwards.db"
    authorization  = Tier3.Authorization unit
    authentication = Tier3.Authentication unit

forwardLinux :: Tier3.Request Unit
forwardLinux = do
  event <- lift $ liftEffect Linux.random
  _     <- Tier3.request settings (Route.Forward (Forward.Linux event)) 
  pure unit 
  where
    settings = Tier3.Settings authorization authentication dbms
    dbms     = Tier3.Local $ "Test.Tier3.forwards.db"
    authorization  = Tier3.Authorization unit
    authentication = Tier3.Authentication unit

forwardReport :: Tier3.Request Unit
forwardReport = do
  event <- lift $ liftEffect Report.random
  _     <- Tier3.request settings (Route.Forward (Forward.Report event)) 
  pure unit 
  where
    settings = Tier3.Settings authorization authentication dbms
    dbms     = Tier3.Local $ "Test.Tier3.forwards.db"
    authorization  = Tier3.Authorization unit
    authentication = Tier3.Authentication unit

forwardWindows :: Tier3.Request Unit
forwardWindows = do
  event <- lift $ liftEffect Windows.random
  _     <- Tier3.request settings (Route.Forward (Forward.Windows event)) 
  pure unit 
  where
    settings = Tier3.Settings authorization authentication dbms
    dbms     = Tier3.Local $ "Test.Tier3.forwards.db"
    authorization  = Tier3.Authorization unit
    authentication = Tier3.Authentication unit
   
forward :: Tier3.Request Unit
forward = do
  choice <- lift $ liftEffect (Math.floor <$> ((*) 6.0) <$> Math.random)
  case choice of
    0 -> forwardAudit
    1 -> forwardAlert
    2 -> forwardFlow
    3 -> forwardReport
    4 -> forwardLinux
    _ -> forwardWindows 

forwards :: Tier3.Request Unit
forwards = void $ sequence (const forward <$> Array.range 1 1000) 

replications :: Tier3.Request Unit
replications = void $ sequence $ replication <$> Array.range 1 10

requests :: Tier3.Request Unit
requests = void $ sequence $
  [ replications
  , forwards
  ]

main :: Effect Unit
main = void $ launchAff $ do
  result <- Tier3.execute requests
  pure unit
