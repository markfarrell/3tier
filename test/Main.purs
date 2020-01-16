module Test.Main where

import Prelude

import Control.Monad.Error.Class (try, throwError)

import Data.Either (Either(..), isRight)
import Data.Tuple (Tuple(..))

import Effect (Effect)
import Effect.Aff (Aff, launchAff, killFiber)
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

assert' :: forall a b. String -> Either a b -> Aff Unit
assert' label result = assert label true $ isRight result

testRequest :: forall a. String -> DB.Request a -> Aff a
testRequest label request = do
  result <- DB.runRequest request
  _      <- assert' label result
  case result of
    (Left error)            -> throwError error
    (Right (Tuple x steps)) -> pure x

testSchema :: String -> Aff Unit
testSchema filename = assert' label =<< try do
  _ <- testRequest "Test.DB.touch"                    $ DB.touch filename
  _ <- testRequest "Test.DB.remove"                   $ remove' filename
  _ <- testRequest "Test.SIEM.Logging.Sensor.schema"  $ Sensor.schema filename
  _ <- testRequest "Test.SIEM.Logging.Linux.schema"   $ Linux.schema filename
  _ <- testRequest "Test.SIEM.Logging.Windows.schema" $ Windows.schema filename
  _ <- testRequest "Test.SIEM.Logging.Audit.schema"   $ Audit.schema filename
  pure unit
  where 
    remove' filename' = do
      _ <- DB.remove filename' $ "Sensor"
      _ <- DB.remove filename' $ "Windows"
      _ <- DB.remove filename' $ "Linux" 
      _ <- DB.remove filename' $"Audit"
      pure unit
    label = "Test.DB.schema"

testServer' :: Aff Unit
testServer' = assert' label =<< try do
  server <- liftEffect (HTTP.createServer)
  _      <- liftEffect (HTTP.listen port $ server)
  _      <- liftEffect (HTTP.close server)
  pure unit
  where
    port  = 4000
    label = "Test.HTTP.Server" 

testServer :: Aff Unit
testServer = assert' label =<< try do
  server <- liftEffect (HTTP.createServer)
  fiber  <- Server.start server
  _      <- liftEffect (HTTP.listen port $ server)
  _      <- flip killFiber fiber $ error "Expected behaviour."
  _      <- liftEffect (HTTP.close server)
  pure unit
  where
    port  = 4000
    label = "Test.SIEM.Logging.Server"

testParseSensor :: String -> Aff Sensor.Entry
testParseSensor entry = do
  result <- pure $ runParser entry Sensor.parseEntry
  _      <- assert' label $ result
  case result of
    (Left _)       -> throwError $ error "Unexpected behaviour."
    (Right entry') -> pure entry'
  where label = "Test.SIEM.Logging.Sensor.parseEntry"

testWriteSensor :: Sensor.Entry -> Aff String
testWriteSensor entry = do
  result <- pure $ Sensor.writeEntry entry
  _      <- assert' label result
  case result of
    (Left _)       -> throwError $ error "Unexpected behaviour."
    (Right entry') -> pure entry'
  where label = "Test.SIEM.Logging.Sensor.writeEntry"

testForwardSensor :: String -> Aff HTTP.IncomingResponse
testForwardSensor entry = do
  server <- liftEffect (HTTP.createServer)
  fiber  <- Server.start server
  _      <- liftEffect (HTTP.listen port $ server)
  result <- Forwarder.forwardSensor host entry
  _      <- assert label ok $ statusCode' result
  _      <- flip killFiber fiber $ error "Expected behaviour."
  _      <- liftEffect (HTTP.close server)
  pure result
  where 
    ok   = 200
    port = 4000
    host = "127.0.0.1:4000"
    label = "Test.SIEM.Logging.Forwarder.forwardSensor"
    statusCode' (HTTP.IncomingResponse _ req) = HTTP.statusCode req

testInsertSensor :: String -> Sensor.Entry -> HTTP.IncomingMessage -> Aff Unit
testInsertSensor filename entry req = testRequest label $ Sensor.insert filename entry req
  where label = "Test.SIEM.Logging.Sensor.insert"

testTotalSensor :: String -> Aff Number
testTotalSensor filename = do
  total   <- testRequest label $ Sensor.average filename
  _       <- assert label' expect $ total
  pure total
  where
    expect = 1.0
    label  = "Test.SIEM.Logging.Sensor.total (1)"
    label' = "Test.SIEM.Logging.Sensor.total (2)"


testAverageSensor :: String -> Aff Number
testAverageSensor filename = do
  average <- testRequest label $ Sensor.average filename
  _       <- assert label' expect $ average
  pure average
  where
    expect = 1.0
    label  = "Test.SIEM.Logging.Sensor.average (1)"
    label' = "Test.SIEM.Logging.Sensor.average (2)"

testVarianceSensor :: String -> Aff Number
testVarianceSensor filename = do
  variance <- testRequest label $ Sensor.variance filename
  _        <- assert label' expect $ variance
  pure variance
  where
    expect = 0.0
    label  = "Test.SIEM.Logging.Sensor.variance (1)"
    label' = "Test.SIEM.Logging.Sensor.variance (2)"

testMinSensor :: String -> Aff Number
testMinSensor filename = do
  min'  <- testRequest label $ Sensor.min filename
  _     <- assert label' expect $ min'
  pure min'
  where
    expect = 1.0
    label  = "Test.SIEM.Logging.Sensor.min (1)"
    label' = "Test.SIEM.Logging.Sensor.min (2)"

testSensor :: String -> Aff Unit
testSensor filename = assert' label  =<< try do
  entry'  <- testParseSensor entry
  entry'' <- testWriteSensor entry'
  req     <- (\(HTTP.IncomingResponse _ req) -> req) <$> testForwardSensor entry''
  _       <- testInsertSensor filename entry' $ req
  _       <- testTotalSensor filename
  _       <- testAverageSensor filename
  _       <- testVarianceSensor filename
  _       <- testMinSensor filename
  pure unit
  where
    entry  = "192.168.2.100,192.168.2.200,3000,37396,6,32,2888,FSPA,2019/12/28T18:58:08.804,0.084,2019/12/28T18:58:08.888,local"
    label  = "Test.SIEM.Logging.Sensor"

main :: Effect Unit
main = void $ launchAff $ do
  _ <- testSchema filename
  _ <- testServer'
  _ <- testServer
  _ <- testSensor filename
  pure unit
  where filename = "test.db"
