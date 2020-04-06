module Test.Tier3 where

import Prelude

import Data.Array as Array
import Data.Traversable (sequence)

import Effect (Effect)
import Effect.Aff (launchAff)

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
    range    m     = case m > 0 of
                       true  -> Array.range 1 n
                       false -> []
    authorization  = Tier3.Authorization unit
    authentication = Tier3.Authentication unit

none :: Tier3.Request Unit
none = do
  _ <- nothing
  _ <- replication 0
  pure unit

single :: Tier3.Request Unit
single = do
  _ <- nothing
  _ <- replication 0
  _ <- replication 1
  pure unit

many :: Tier3.Request Unit
many = do
  _ <- nothing
  _ <- replication 0
  _ <- replication 1
  _ <- replication 10
  _ <- replication 100
  pure unit

request :: Tier3.Request Unit
request = void $ sequence [nothing, none, single, many]

main :: Effect Unit
main = void $ launchAff $ Tier3.execute request
