module Test.Tier2 where

import Prelude

import Control.Coroutine as Coroutine

import Data.Either(Either)

import Effect (Effect)
import Effect.Aff (Aff, launchAff, forkAff, supervise)
import Effect.Class (liftEffect)
import Effect.Exception (Error)

import Date as Date
import HTTP as HTTP

import IPv4 (IPv4(..))

import Flow as Flow
import Forward as Forward
import Report as Report

import Route (Route)
import Route as Route

import Tier2 as Tier2
import Tier3 as Tier3

import Test.UnitTest (UnitTest(..))
import Test.UnitTest as UnitTest

execute :: Tier3.Settings -> Tier2.Settings -> Route -> Aff (Either Error HTTP.IncomingResponse)
execute settings (Tier2.Settings settings') request = do
  server <- liftEffect (HTTP.createServer)
  fiber  <- forkAff (Coroutine.runProcess $ Tier2.process settings server)
  _      <- liftEffect (HTTP.listen settings'.port $ server)
  result <- Tier2.execute (Tier2.Settings settings') request
  _      <- liftEffect (HTTP.close server)
  pure result

execute' :: Route -> Aff (Either Error HTTP.IncomingResponse)
execute' = execute settings settings'
  where
    settings = Tier3.Settings $
      [ Tier3.Local "Test.Tier2.db.1"
      , Tier3.Local "Test.Tier2.db.2"
      ]
    settings' = Tier2.Settings $
      { host : "localhost"
      , port : 4000
      }

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
      [ Route.Forward $ Forward.Flow $ Flow.Record $
        { sourceIPv4 : IPv4 0 0 0 0
        , destinationIPv4 : IPv4 0 0 0 0
        , sourcePort : 0
        , destinationPort : 0
        , protocol : 0
        , packets : 0
        , bytes : 0
        , flags : ""
        , startTime : Date.epoch
        , duration : 0.000
        , endTime : Date.epoch
        }
      , Route.Forward $ Forward.Flow $ Flow.Record $
        { sourceIPv4 : IPv4 255 255 255 255
        , destinationIPv4 : IPv4 255 255 255 255
        , sourcePort : 65535
        , destinationPort : 65535
        , protocol : 255
        , packets : 123456789
        , bytes : 123456789
        , flags : "URFSPA"
        , startTime : Date.epoch
        , duration : 0.000
        , endTime : Date.epoch
        }
      ]

reportAudit :: UnitTest Route Error HTTP.IncomingResponse
reportAudit = UnitTest $
  { testSuite    : "Test.Tier2"
  , testName     : "Tier2.execute"
  , testCase     : "/report/audit/*/*/*"
  , testFunction : execute'
  , testInputs   : Route.Report <$> Report.reports
  }

unitTests :: Aff Unit
unitTests = supervise $ do
  _ <- UnitTest.execute forwardFlow
  _ <- UnitTest.execute reportAudit
  pure unit

main :: Effect Unit
main = void $ launchAff $ unitTests

