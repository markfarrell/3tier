module Test.Main where

import Prelude

import Control.Coroutine (runProcess)
import Control.Monad.Error.Class (try)

import Data.Either (Either, isRight)

import Effect (Effect)
import Effect.Aff (Aff, launchAff, forkAff, killFiber)
import Effect.Class (liftEffect)

import Effect.Exception (error)

import HTTP as HTTP

import DB as DB

import SIEM.Logging.Sensor as Sensor
import SIEM.Logging.Linux as Linux
import SIEM.Logging.Windows as Windows

import SIEM.Logging.Forwarder as Forwarder
import SIEM.Logging.Server as Server

import Audit as Audit

import Assert (assert)

assert' :: forall a b. Either a b -> Aff Unit
assert' result = assert true $ isRight result

testRequest :: forall a. DB.Request a -> Aff Unit
testRequest request = assert' =<< DB.runRequest request

testSchema :: Aff Unit
testSchema = assert' =<< try do
  _ <- testRequest $ DB.touch filename
  _ <- testRequest $ Sensor.schema filename
  _ <- testRequest $ Linux.schema filename
  _ <- testRequest $ Windows.schema filename
  _ <- testRequest $ Audit.schema filename
  pure unit
  where filename = "test.db"

testServer':: Aff Unit
testServer' = assert' =<< try do
  server <- liftEffect (HTTP.createServer)
  _      <- liftEffect (HTTP.listen port $ server)
  _      <- liftEffect (HTTP.close server)
  pure unit
  where port = 4000

testServer:: Aff Unit
testServer = assert' =<< try do
  server <- liftEffect (HTTP.createServer)
  fiber  <- forkAff $ runProcess (Server.process server)
  _      <- liftEffect (HTTP.listen port $ server)
  _      <- flip killFiber fiber $ error "Expected behaviour."
  _      <- liftEffect (HTTP.close server)
  pure unit
  where port = 4000
        
testForwardSensor:: Aff Unit
testForwardSensor = assert' =<< try do
  server <- liftEffect (HTTP.createServer)
  fiber  <- forkAff $ runProcess (Server.process server)
  _      <- liftEffect (HTTP.listen port $ server)
  result <- statusCode' <$> Forwarder.forwardSensor host entry
  _      <- assert ok result
  _      <- flip killFiber fiber $ error "Expected behaviour."
  _      <- liftEffect (HTTP.close server)
  pure unit
  where 
    ok   = 200
    port = 4000
    host = "127.0.0.1:4000"
    entry = "0.0.0.0,0.0.0.1,3000,37396,6,32,2888,FSPA,2019/12/28T18:58:08.804,0.084,2019/12/28T18:58:08.888,local"
    statusCode' (HTTP.IncomingResponse _ req) = HTTP.statusCode req

main :: Effect Unit
main = void $ launchAff $ do
  _ <- testSchema
  _ <- testServer'
  _ <- testServer
  _ <- testForwardSensor
  pure unit
