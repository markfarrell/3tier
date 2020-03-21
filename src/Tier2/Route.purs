module Tier2.Route
  ( Route(..)
  , execute
  ) where

import Prelude

import Data.Either (Either(..))

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error)

import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.String (string)
import Text.Parsing.Parser.Combinators (choice)

import Audit as Audit
import Flow as Flow
import Report as Report

import Date as Date
import HTTP as HTTP
import Strings as Strings

import Tier3 as Tier3

data Route = Forward Tier3.Query | Report Tier3.Query

reportAudit'' :: Audit.EventID -> String
reportAudit'' Audit.DatabaseRequest = "database-request"
reportAudit'' Audit.ResourceRequest = "resource-request"
reportAudit'' Audit.RoutingRequest  = "routing-request"

reportAudit' :: Audit.EventID -> Parser String Route
reportAudit' eventID = choice [w, x, y, z] 
  where
    w = do
      _ <- string ("/report/audit/" <> reportAudit'' eventID <> "/success/sources")
      pure (Report (Tier3.SelectQuery $ Report.Audit eventID Audit.Success Report.Sources))
    x = do
      _ <- string ("/report/audit/" <> reportAudit'' eventID <> "/failure/sources")
      pure (Report (Tier3.SelectQuery $ Report.Audit eventID Audit.Failure Report.Sources))
    y = do
      _ <- string ("/report/audit/" <> reportAudit'' eventID <> "/success/durations")
      pure (Report (Tier3.SelectQuery $ Report.Audit eventID Audit.Success Report.Durations))
    z = do
      _ <- string ("/report/audit/" <> reportAudit'' eventID <> "/failure/durations")
      pure (Report (Tier3.SelectQuery $ Report.Audit eventID Audit.Failure Report.Durations))

reportAudit :: Parser String Route
reportAudit = choice $ reportAudit' <$> [Audit.DatabaseRequest, Audit.ResourceRequest, Audit.RoutingRequest] 

report :: Parser String Route
report = choice [reportAudit]

forwardFlow :: Parser String Route
forwardFlow = do
  _     <- string "/forward/flow/"
  entry <- Flow.flow
  pure (Forward (Tier3.InsertQuery $ Tier3.InsertFlow entry))

forward :: Parser String Route
forward = choice [forwardFlow] 

route :: Parser String Route
route = choice [forward, report]

routingRequest :: Either Error Route -> Number -> Audit.Entry
routingRequest (Left _)                                                    = \duration -> Audit.Entry Audit.Failure Audit.RoutingRequest duration "???"               
routingRequest (Right (Forward (Tier3.InsertQuery (Tier3.InsertFlow _))))  = \duration -> Audit.Entry Audit.Success Audit.RoutingRequest duration "FORWARD-FLOW"
routingRequest (Right (Forward _))                                         = \duration -> Audit.Entry Audit.Success Audit.RoutingRequest duration "???"
routingRequest (Right (Report (Tier3.SelectQuery (Report.Audit _ _ _))))   = \duration -> Audit.Entry Audit.Success Audit.RoutingRequest duration "REPORT-AUDIT"
routingRequest (Right (Report _))                                          = \duration -> Audit.Entry Audit.Success Audit.RoutingRequest duration "???"

audit :: Tier3.Settings -> Either Error Route -> Number -> HTTP.IncomingMessage -> Aff Unit
audit settings result duration req = do
  entry <- pure $ routingRequest result duration
  _ <- Tier3.execute $ Tier3.request settings (Tier3.InsertQuery $ Tier3.InsertAudit entry) req
  pure unit

execute' :: HTTP.IncomingMessage -> Aff (Either Error Route)
execute' req = do 
  result <- pure (flip runParser route $ Strings.decodeURIComponent (HTTP.messageURL req)) 
  case result of
    (Left _)        -> pure (Left $ error "Invalid routing request.")
    (Right result') -> pure (Right $ result')

execute :: Tier3.Settings -> HTTP.IncomingMessage -> Aff (Either Error Route)
execute settings req = do 
  startTime <- liftEffect $ Date.currentTime
  result    <- execute' req
  endTime   <- liftEffect $ Date.currentTime
  duration  <- pure $ endTime - startTime
  _         <- audit settings result duration req 
  pure result
