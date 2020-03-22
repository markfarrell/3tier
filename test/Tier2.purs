module Test.Tier2 where

import Prelude

import Control.Coroutine as Coroutine

import Data.Either(Either)

import Effect (Effect)
import Effect.Aff (Aff, launchAff, forkAff, killFiber)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error)

import HTTP as HTTP

import Audit as Audit
import Flow as Flow
import Forward as Forward
import Report as Report

import Tier2 as Tier2
import Tier2.Route (Route)
import Tier2.Route as Route

import Tier3 as Tier3

import Test.UnitTest (UnitTest(..))
import Test.UnitTest as UnitTest

execute :: Tier3.Settings -> Tier2.Settings -> Route -> Aff (Either Error HTTP.IncomingResponse)
execute settings (Tier2.Settings tier2) request = do
  server <- liftEffect (HTTP.createServer)
  fiber  <- forkAff (Coroutine.runProcess $ Tier2.process settings server)
  _      <- liftEffect (HTTP.listen tier2.port $ server)
  result <- Tier2.execute (Tier2.Settings tier2) request
  _      <- flip killFiber fiber $ error "Expected behaviour."
  _      <- liftEffect (HTTP.close server)
  pure result

execute' :: Route -> Aff (Either Error HTTP.IncomingResponse)
execute' = execute "Test.Tier2.db" (Tier2.Settings { host : "localhost", port : 4000 })

forwardFlow :: UnitTest Route Error HTTP.IncomingResponse
forwardFlow = UnitTest $
  { testSuite    : "Test.Tier2"
  , testName     : "Tier2.execute"
  , testCase     : "/forward/flow/*"
  , testFunction : execute'
  , testInputs   : testInputs
  }
  where
    testInputs =
      [ Route.Forward $ Forward.Flow $ Flow.Entry $
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
      , Route.Forward $ Forward.Flow $ Flow.Entry $
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

reportAudit :: UnitTest Route Error HTTP.IncomingResponse
reportAudit = UnitTest $
  { testSuite    : "Test.Tier2"
  , testName     : "Tier2.execute"
  , testCase     : "/report/audit/*/*/*"
  , testFunction : execute'
  , testInputs   : testInputs
  }
  where
    testInputs = 
      [ Route.Report $ Report.Audit Audit.DatabaseRequest Audit.Success Report.Sources
      , Route.Report $ Report.Audit Audit.DatabaseRequest Audit.Failure Report.Sources
      , Route.Report $ Report.Audit Audit.DatabaseRequest Audit.Success Report.Durations
      , Route.Report $ Report.Audit Audit.DatabaseRequest Audit.Failure Report.Durations
      , Route.Report $ Report.Audit Audit.ResourceRequest Audit.Success Report.Sources
      , Route.Report $ Report.Audit Audit.ResourceRequest Audit.Failure Report.Sources
      , Route.Report $ Report.Audit Audit.ResourceRequest Audit.Success Report.Durations
      , Route.Report $ Report.Audit Audit.ResourceRequest Audit.Failure Report.Durations
      , Route.Report $ Report.Audit Audit.RoutingRequest Audit.Success Report.Sources
      , Route.Report $ Report.Audit Audit.RoutingRequest Audit.Failure Report.Sources
      , Route.Report $ Report.Audit Audit.RoutingRequest Audit.Success Report.Durations
      , Route.Report $ Report.Audit Audit.RoutingRequest Audit.Failure Report.Durations
      ]

main :: Effect Unit
main = void $ launchAff $ do
  _ <- UnitTest.execute forwardFlow
  _ <- UnitTest.execute reportAudit
  pure unit
