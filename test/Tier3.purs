module Test.Tier3 where

import Prelude

import Control.Monad.Trans.Class (lift)

import Data.Array as Array
import Data.Traversable (sequence)

import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Class (liftEffect)

import Effect.Flow (random) as Flow

import Forward as Forward

import Route as Route

import Report as Report

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

forward :: Tier3.Request Unit
forward = do
  flow <- lift $ liftEffect Flow.random
  _    <- Tier3.request settings (Route.Forward (Forward.Flow flow)) 
  pure unit 
  where
    settings = Tier3.Settings authorization authentication dbms
    dbms     = Tier3.Local $ "Test.Tier3.forward.db"
    authorization  = Tier3.Authorization unit
    authentication = Tier3.Authentication unit

request :: Tier3.Request Unit
request = void $ sequence $ replication <$> Array.range 1 10

main :: Effect Unit
main = void $ launchAff $ Tier3.execute request
