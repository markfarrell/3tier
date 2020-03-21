module Test.Tier2 where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, launchAff, forkAff, killFiber)
import Effect.Class (liftEffect)
import Effect.Exception (error)

import Assert (assert)
import HTTP as HTTP
import Strings as Strings

import Tier2 as Tier2

forwardFlow :: String -> String -> Aff HTTP.IncomingResponse
forwardFlow host query = do
  req <- HTTP.createRequest HTTP.Post url
  res <- HTTP.endRequest req
  pure res
  where url = "http://" <> host <> "/forward/flow?q=" <> (Strings.encodeURIComponent query)

testForwardFlow' :: String -> Aff HTTP.IncomingResponse
testForwardFlow' query = do
  server <- liftEffect (HTTP.createServer)
  fiber  <- forkAff $ Tier2.start settings server
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
    label = "/forward/flow (" <> query <> ")"
    statusCode' (HTTP.IncomingResponse _ req) = HTTP.statusCode req
    settings = "Test.Tier2.db"

testForwardFlow :: Aff Unit
testForwardFlow = do
  _  <- testForwardFlow' query
  _  <- testForwardFlow' query'
  pure unit
  where
    query   = "0.0.0.0,0.0.0.0,0,65535,6,32,2888,FSPA,2019/12/28T18:58:08.804,0.084,2019/12/28T18:58:08.888,local"
    query'  = "0.0.0.0,0.12.123.255,0,65535,6,32,2888,FSPA,1970-01-01T00:00:00.000Z,0.084,2019/12/28T18:58:08.888,local"

main :: Effect Unit
main = void $ launchAff $ do
  _ <- testForwardFlow
  pure unit
