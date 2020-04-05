module Test.Tier3 where

import Prelude

import Data.Traversable (sequence)

import Effect (Effect)
import Effect.Aff (Aff, launchAff, supervise)

import Report as Report

import Route (Route)
import Route as Route

import Audit as Audit

import Tier3 as Tier3

import Test.Test (Test(..))
import Test.Test as Test

test :: Route -> Test Tier3.Resource
test = \route -> Test $
  { eventType     : Audit.Success
  , eventCategory : Audit.Tier3
  , eventID       : case route of
                      (Route.Forward _) -> Audit.Forward
                      (Route.Report  _) -> Audit.Report
  , eventURI      : Route.uri route 
  , execute       : \_ -> Tier3.execute $ Tier3.request settings route 
  }
  where
    settings = Tier3.Settings (Tier3.Local <$> ["Test.Tier3.db.1", "Test.Tier3.db.2"])

tests :: Array (Test Tier3.Resource) 
tests = do 
  reports <- test <$> Route.Report <$> Report.sample
  pure reports

execute :: Aff Unit
execute = supervise $ do
  _ <- sequence (Test.execute <$> tests)
  pure unit

main :: Effect Unit
main = void $ launchAff execute

