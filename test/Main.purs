module Test.Main where

import Prelude

import Data.Either (isRight)

import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)

import HTTP as HTTP

import DB as DB

import SIEM.Logging.Sensor as Sensor
import SIEM.Logging.Linux as Linux
import SIEM.Logging.Windows as Windows

import Audit as Audit

import Assert (assert)

runRequest' :: forall a. DB.Request a -> Aff Unit
runRequest' request = do
  result <- DB.runRequest request
  assert true $ isRight result

testSchema :: Aff Unit
testSchema = do
  _ <- runRequest' $ DB.touch filename
  _ <- runRequest' $ Sensor.schema filename
  _ <- runRequest' $ Linux.schema filename
  _ <- runRequest' $ Windows.schema filename
  _ <- runRequest' $ Audit.schema filename
  pure unit
  where filename = "test.db"

testServer':: Aff Unit
testServer' = do
  server <- liftEffect (HTTP.createServer)
  _      <- liftEffect (HTTP.listen port $ server)
  _      <- liftEffect (HTTP.close server)
  pure unit
  where port = 4000

main :: Effect Unit
main = void $ launchAff $ do
  _ <- testSchema
  _ <- testServer'
  pure unit
