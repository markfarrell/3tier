module Test.Main where

import Prelude

import Data.Either (isRight)

import Effect (Effect)
import Effect.Aff (Aff, launchAff)

import Assert (assert)

import DB as DB

import SIEM.Logging.Sensor as Sensor
import SIEM.Logging.Linux as Linux
import SIEM.Logging.Windows as Windows

import Audit as Audit

runRequest' :: forall a. DB.Request a -> Aff Unit
runRequest' request = do
  result <- DB.runRequest request
  assert true $ isRight result

main :: Effect Unit
main = void $ launchAff $ do
  _ <- runRequest' $ DB.touch ":memory:"
  _ <- runRequest' $ DB.touch filename
  _ <- runRequest' $ Sensor.schema filename
  _ <- runRequest' $ Linux.schema filename
  _ <- runRequest' $ Windows.schema filename
  _ <- runRequest' $ Audit.schema filename
  pure unit
  where filename = "test.db"
