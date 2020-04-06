module Test.Tier3 where

import Prelude

import Control.Monad.Trans.Class (lift)

import Data.Array as Array
import Data.Traversable (sequence)

import Effect (Effect)
import Effect.Aff (launchAff)

import Data.Schema as Schema

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
    settings   = Tier3.Settings $ setting <$> range n
    setting  m = Tier3.Local $ "Test.Tier3.replication.db." <> show m
    routes     = Route.Report <$> Report.all
    range    m = case m > 0 of
                 true  -> Array.range 1 n
                 false -> []

none :: Tier3.Request Unit
none = do
  _ <- nothing
  _ <- replication 0
  pure unit

one :: Tier3.Request Unit
one = do
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
request = void $ sequence [nothing, none, one, many]

main :: Effect Unit
main = void $ launchAff $ Tier3.execute request
