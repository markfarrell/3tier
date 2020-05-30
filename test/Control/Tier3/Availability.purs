module Test.Control.Tier3.Availability 
  ( suite
  ) where

import Prelude

import Control.Monad.Trans.Class (lift)

import Data.Array as Array
import Data.Either (Either(..))
import Data.Traversable (sequence)

import Effect.Aff (Aff)
import Effect.Console (log) as Console
import Effect.Class (liftEffect)
import Effect.Exception (throw) as Exception

import FFI.Date as Date
import FFI.Math as Math
import FFI.FS as FS

import Control.Authorization as Authorization
import Control.Authentication as Authentication

import Data.Route as Route

import Control.Tier3 as Tier3

import Effect.Forward (random) as Forward

import Test.Data.Test as Test

forward :: Tier3.Settings -> Tier3.Request Unit
forward settings  = do
  event <- lift $ liftEffect Forward.random
  _     <- Tier3.request settings (Route.Forward event) 
  pure unit 

forwards :: Tier3.Settings -> Int -> Tier3.Request Unit
forwards = \settings n ->  do
  _ <- sequence (const (forward settings) <$> Array.range 1 n) 
  pure unit

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
    , startTime     : startTime
    , duration      : duration
    , endTime       : endTime
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
    , startTime     : startTime
    , duration      : duration
    , endTime       : endTime
    }
  _     <- lift $ liftEffect (Console.log $ show event)
  pure unit
  where 
    settings = Tier3.Settings Authorization.Default Authentication.Default dbms
    dbms     = Tier3.Replication $ Tier3.Primary Tier3.Testing
    n        = 1

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
    , startTime     : startTime
    , duration      : duration
    , endTime       : endTime
    }
  _     <- lift $ liftEffect (Console.log $ show event)
  pure unit
  where 
    settings = Tier3.Settings Authorization.Default Authentication.Default dbms
    dbms     = Tier3.Failover $ Tier3.Primary Tier3.Testing
    n        = 1

tests :: Tier3.Request Unit
tests = void $ sequence $
  [ testSingleForward
  , testReplicationForward
  , testFailoverForwards
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
