module Test.Main where

import Prelude

import Data.Either (isRight)

import Effect (Effect)
import Effect.Aff (Aff, launchAff)

import Assert (assert)

import DB as DB
import SIEM.Logging.Sensor as Sensor
import SIEM.Logging.Linux as Linux

runRequest' :: forall a. DB.Request a -> Aff Unit
runRequest' request = do
  result <- DB.runRequest request
  assert true $ isRight result

main :: Effect Unit
main = void $ launchAff $ do
  _ <- runRequest' $ DB.touch ":memory:"
  _ <- runRequest' $ DB.touch "test.db"
  _ <- runRequest' $ Sensor.schema "test.db"
  _ <- runRequest' $ Linux.schema "test.db"
  pure unit
