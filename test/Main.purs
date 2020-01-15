module Test.Main where

import Prelude

import Control.Coroutine (runProcess)
import Control.Monad.Error.Class (try, throwError)

import Data.Either (Either(..), isRight)

import Effect (Effect)
import Effect.Aff (Aff, launchAff, forkAff, killFiber)
import Effect.Class (liftEffect)

import Effect.Exception (error)

import Text.Parsing.Parser (runParser)

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

testSchema :: String -> Aff Unit
testSchema filename = assert' =<< try do
  _ <- testRequest $ DB.touch filename
  _ <- testRequest $ Sensor.schema filename
  _ <- testRequest $ Linux.schema filename
  _ <- testRequest $ Windows.schema filename
  _ <- testRequest $ Audit.schema filename
  pure unit

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

testParseSensor :: String -> Aff Sensor.Entry
testParseSensor entry = do
  result <- pure $ runParser entry Sensor.parseEntry
  _      <- assert' result
  case result of
    (Left _)       -> throwError $ error "Unexpected behaviour."
    (Right entry') -> pure entry'

testWriteSensor :: Sensor.Entry -> Aff String
testWriteSensor entry = do
  result <- pure $ Sensor.writeEntry entry
  _      <- assert' result
  case result of
    (Left _)       -> throwError $ error "Unexpected behaviour."
    (Right entry') -> pure entry'

testForwardSensor :: String -> Aff HTTP.IncomingResponse
testForwardSensor entry = do
  server <- liftEffect (HTTP.createServer)
  fiber  <- forkAff $ runProcess (Server.process server)
  _      <- liftEffect (HTTP.listen port $ server)
  result <- Forwarder.forwardSensor host entry
  _      <- assert ok $ statusCode' result
  _      <- flip killFiber fiber $ error "Expected behaviour."
  _      <- liftEffect (HTTP.close server)
  pure result
  where 
    ok   = 200
    port = 4000
    host = "127.0.0.1:4000"
    statusCode' (HTTP.IncomingResponse _ req) = HTTP.statusCode req

testInsertSensor :: String -> Sensor.Entry -> HTTP.IncomingMessage -> Aff Unit
testInsertSensor filename entry req = testRequest $ Sensor.insert filename entry req

testSensor :: String -> Aff Unit
testSensor filename = assert' =<< try do
  entry'  <- testParseSensor entry
  entry'' <- testWriteSensor entry'
  req     <- (\(HTTP.IncomingResponse _ req) -> req) <$> testForwardSensor entry''
  _       <- testInsertSensor filename entry' $ req
  pure unit
  where entry = "192.168.2.100,192.168.2.200,3000,37396,6,32,2888,FSPA,2019/12/28T18:58:08.804,0.084,2019/12/28T18:58:08.888,local"

main :: Effect Unit
main = void $ launchAff $ do
  _ <- testSchema filename
  _ <- testServer'
  _ <- testServer
  _ <- testSensor filename
  pure unit
  where filename = "test.db"
