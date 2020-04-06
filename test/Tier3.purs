module Test.Tier3 where

import Prelude

import Control.Monad.Trans.Class (lift)

import Data.Array as Array
import Data.Traversable (sequence)

import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)

import Effect.Flow (random) as Flow
import Effect.Linux (random) as Linux
import Effect.Windows (random) as Windows

import Tier.Forward as Forward
import Tier.Report as Report
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

forwardFlow :: Tier3.Request Unit
forwardFlow = do
  event <- lift $ liftEffect Flow.random
  _     <- Tier3.request settings (Route.Forward (Forward.Flow event)) 
  pure unit 
  where
    settings = Tier3.Settings authorization authentication dbms
    dbms     = Tier3.Local $ "Test.Tier3.forwardFlow.db"
    authorization  = Tier3.Authorization unit
    authentication = Tier3.Authentication unit

forwardLinux :: Tier3.Request Unit
forwardLinux = do
  event <- lift $ liftEffect Linux.random
  _     <- Tier3.request settings (Route.Forward (Forward.Linux event)) 
  pure unit 
  where
    settings = Tier3.Settings authorization authentication dbms
    dbms     = Tier3.Local $ "Test.Tier3.forwardLinux.db"
    authorization  = Tier3.Authorization unit
    authentication = Tier3.Authentication unit

forwardWindows :: Tier3.Request Unit
forwardWindows = do
  event <- lift $ liftEffect Windows.random
  _     <- Tier3.request settings (Route.Forward (Forward.Windows event)) 
  pure unit 
  where
    settings = Tier3.Settings authorization authentication dbms
    dbms     = Tier3.Local $ "Test.Tier3.forwardWindows.db"
    authorization  = Tier3.Authorization unit
    authentication = Tier3.Authentication unit

replications :: Tier3.Request Unit
replications = void $ sequence $ replication <$> Array.range 1 10

requests :: Tier3.Request Unit
requests = void $ sequence $
  [ replications
  , forwardFlow
  , forwardLinux
  , forwardWindows
  ]

main :: Effect Unit
main = void $ launchAff $ Tier3.execute requests
