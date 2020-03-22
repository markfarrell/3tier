module Test.Tier2 where

import Prelude

import Control.Coroutine as Coroutine

import Data.Either(Either)

import Effect (Effect)
import Effect.Aff (Aff, launchAff, forkAff, killFiber)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error)

import HTTP as HTTP

import Flow as Flow

import Tier2 as Tier2
import Tier3 as Tier3

import Test.UnitTest as UnitTest

forwardFlow :: Tier3.Settings -> Tier2.Settings -> Flow.Entry -> Aff (Either Error HTTP.IncomingResponse)
forwardFlow settings (Tier2.Settings tier2) entry = do
  server <- liftEffect (HTTP.createServer)
  fiber  <- forkAff (Coroutine.runProcess $ Tier2.process settings server)
  _      <- liftEffect (HTTP.listen tier2.port $ server)
  result <- Tier2.forwardFlow (Tier2.Settings tier2) entry
  _      <- flip killFiber fiber $ error "Expected behaviour."
  _      <- liftEffect (HTTP.close server)
  pure result

forwardFlow' :: Flow.Entry -> Aff (Either Error HTTP.IncomingResponse)
forwardFlow' = forwardFlow "Test.Tier2.db" (Tier2.Settings { host : "localhost", port : 4000 })

testCases :: Array Flow.Entry
testCases =
  [ Flow.Entry $
    { sIP : "0.0.0.0"
    , dIP : "0.0.0.0"
    , sPort : "0"
    , dPort : "0"
    , protocol : "0"
    , packets : "0"
    , bytes : "0"
    , flags : ""
    , sTime : "1970-01-01T00:00:00.000Z"
    , duration : 0.000
    , eTime : "1970-01-01T00:00:00.000Z"
    , sensor : ""
    }
  , Flow.Entry $
    { sIP : "255.255.255.255"
    , dIP : "255.255.255.255"
    , sPort : "65535"
    , dPort : "65535"
    , protocol : "255"
    , packets : "123456789"
    , bytes : "123456789"
    , flags : "URFSPA"
    , sTime : "2020-03-22T23:59:59.999Z"
    , duration : 0.000
    , eTime : "2020-03-22T23:59:59.999Z"
    , sensor : "AaBbCcDdEeFf123456789"
    }
  ]

main :: Effect Unit
main = void $ launchAff $ do
  _ <- UnitTest.execute "Test.Tier2" "/forward/flow" forwardFlow' $ testCases
  pure unit
