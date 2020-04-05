module Test.Tier3 where

import Prelude

import Data.Traversable (sequence)

import Effect (Effect)
import Effect.Aff (Aff, launchAff, supervise)

import Report as Report

import Route (Route)
import Route as Route

import Tier3 as Tier3

import Test.UnitTest (UnitTest(..))
import Test.UnitTest as UnitTest

unitTest :: Route -> UnitTest Route Tier3.Resource
unitTest = \route -> UnitTest $
  { suite    : "Tier3.execute"
  , name     : Route.uri route 
  , input    : route
  , expect   : UnitTest.Success
  , execute  : Tier3.execute <<< Tier3.request settings
  }
  where
    settings = Tier3.Settings (Tier3.Local <$> ["Test.Tier3.db.1", "Test.Tier3.db.2"])

unitTests :: Array (UnitTest Route Tier3.Resource) 
unitTests = unitTest <$> Route.Report <$> Report.sample 

execute :: Aff Unit
execute = supervise $ do
  _ <- sequence (UnitTest.execute <$> unitTests)
  pure unit

main :: Effect Unit
main = void $ launchAff execute

