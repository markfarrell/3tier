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
import RSA as RSA

import DB as DB
import Strings as Strings

import Flow as Flow
import Server as Server
import Statistics as Statistics

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

testSchema :: DB.Database -> Aff Unit
testSchema filename = assert' label =<< try do
  _ <- testRequest "Test.DB.touch"          $ DB.touch filename
  _ <- testRequest "Test.DB.remove"         $ remove' filename
  _ <- testRequest "Test.Flow.schema"       $ DB.schema DB.Flow filename
  _ <- testRequest "Test.Audit.schema"      $ DB.schema DB.Audit filename
  pure unit
  where 
    remove' filename' = do
      _ <- DB.remove filename' $ "Flow"
      _ <- DB.remove filename' $ "Audit"
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
    label = "Test.Server"

testParseFlow :: String -> Aff Flow.Entry
testParseFlow entry = do
  result <- pure $ runParser entry Flow.parse
  _      <- assert' label $ result
  case result of
    (Left _)       -> throwError $ error "Unexpected behaviour."
    (Right entry') -> pure entry'
  where label = "Test.Flow.parse"

testUnparseFlow :: Flow.Entry -> Aff String
testUnparseFlow entry = do
  result <- pure $ Flow.unparse entry
  _      <- assert' label result
  case result of
    (Left _)       -> throwError $ error "Unexpected behaviour."
    (Right entry') -> pure entry'
  where label = "Test.Flow.unparse"

forwardFlow :: String -> String -> Aff HTTP.IncomingResponse
forwardFlow host query = do
  req <- HTTP.createRequest HTTP.Post url
  res <- HTTP.endRequest req
  pure res
  where url = "http://" <> host <> "/forward/flow?q=" <> (Strings.encodeURIComponent query)

testForwardFlow :: String -> Aff HTTP.IncomingResponse
testForwardFlow query = do
  server <- liftEffect (HTTP.createServer)
  fiber  <- Server.start server
  _      <- liftEffect (HTTP.listen port $ server)
  result <- forwardFlow host query
  _      <- assert label ok $ statusCode' result
  _      <- flip killFiber fiber $ error "Expected behaviour."
  _      <- liftEffect (HTTP.close server)
  pure result
  where 
    ok   = 200
    port = 4000
    host = "127.0.0.1:4000"
    label = "Test.Test.Forwarder.forwardFlow"
    statusCode' (HTTP.IncomingResponse _ req) = HTTP.statusCode req
      

testInsertFlow :: DB.Database -> Flow.Entry -> HTTP.IncomingMessage -> Aff Unit
testInsertFlow filename entry req = testRequest label $ Flow.insert filename entry req
  where label = "Test.Flow.insert"

testStatistics :: Statistics.Entry -> DB.Database -> DB.Schema -> Aff Statistics.Entry
testStatistics expect filename schema = do
  entry  <- testRequest label $ Statistics.statistics filename schema Statistics.Events
  _      <- assert label' expect $ entry
  pure entry
  where
    label  = "Test.Statistics.statistics " <> show schema <> " (1)"
    label' = "Test.Statistics.statistics " <> show schema <> " (2)"

testFlow :: DB.Database -> Aff Unit
testFlow filename = assert' label  =<< try do
  entry'  <- testParseFlow entry
  entry'' <- testUnparseFlow entry'
  _       <- testStatistics expect filename DB.Flow
  req     <- (\(HTTP.IncomingResponse _ req) -> req) <$> testForwardFlow entry''
  _       <- testInsertFlow filename entry' $ req
  _       <- testStatistics expect' filename DB.Flow
  pure unit
  where
    entry   = "0.0.0.0,0.12.123.255,0,65535,6,32,2888,FSPA,2019/12/28T18:58:08.804,0.084,2019/12/28T18:58:08.888,local"
    label   = "Test.Flow"
    expect = Statistics.Entry $ 
      { min       : 0.0
      , max       : 0.0
      , sum       : 0.0
      , total     : 0.0
      , average   : 0.0
      , variance  : 0.0
      }
    expect' = Statistics.Entry $ 
      { min       : 1.0
      , max       : 1.0
      , sum       : 1.0
      , total     : 1.0
      , average   : 1.0
      , variance  : 0.0
      }

testRSA :: Aff Unit
testRSA = do
  result  <- pure $ RSA.defaultEncrypt expect
  _       <- assert label expect $ RSA.defaultDecrypt result 
  result' <- pure $ RSA.defaultSign expect
  _       <- assert label' true $ RSA.defaultVerify expect result'
  pure unit
  where
    expect = "test"
    label  = "Test.RSA.defaultEncrypt"
    label' = "Test.RSA.defaultSign"

main :: Effect Unit
main = void $ launchAff $ do
  _ <- testRSA
  _ <- testSchema filename
  _ <- testServer'
  _ <- testServer
  _ <- testFlow filename
  pure unit
  where filename = "test.db"
