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
import Forward as Forward
import Flow as Flow
import Report as Report

import Date as Date
import HTTP as HTTP
import Strings as Strings

import Tier3 as Tier3

data Route = Forward Forward.Forward | Report Report.Report

reportAudit'' :: Audit.EventID -> String
reportAudit'' Audit.DatabaseRequest = "database-request"
reportAudit'' Audit.ResourceRequest = "resource-request"
reportAudit'' Audit.RoutingRequest  = "routing-request"

reportAudit' :: Audit.EventID -> Parser String Route
reportAudit' eventID = choice [w, x, y, z] 
  where
    w = do
      _ <- string ("/report/audit/" <> reportAudit'' eventID <> "/success/sources")
      pure (Report (Report.Audit eventID Audit.Success Report.Sources))
    x = do
      _ <- string ("/report/audit/" <> reportAudit'' eventID <> "/failure/sources")
      pure (Report (Report.Audit eventID Audit.Failure Report.Sources))
    y = do
      _ <- string ("/report/audit/" <> reportAudit'' eventID <> "/success/durations")
      pure (Report (Report.Audit eventID Audit.Success Report.Durations))
    z = do
      _ <- string ("/report/audit/" <> reportAudit'' eventID <> "/failure/durations")
      pure (Report (Report.Audit eventID Audit.Failure Report.Durations))

reportAudit :: Parser String Route
reportAudit = choice $ reportAudit' <$> [Audit.DatabaseRequest, Audit.ResourceRequest, Audit.RoutingRequest] 

report :: Parser String Route
report = choice [reportAudit]

forwardFlow :: Parser String Route
forwardFlow = do
  _     <- string "/forward/flow/"
  entry <- Flow.flow
  pure (Forward (Forward.Flow entry))

forward :: Parser String Route
forward = choice [forwardFlow] 

route :: Parser String Route
route = choice [forward, report]

routingRequest :: Either Error Route -> Number -> HTTP.IncomingMessage -> Audit.Entry
routingRequest (Left _)                              = \duration req -> Audit.entry Audit.Failure Audit.RoutingRequest duration "???" $ req 
routingRequest (Right (Forward (Forward.Flow _)))    = \duration req -> Audit.entry Audit.Success Audit.RoutingRequest duration "FORWARD-FLOW" $ req
routingRequest (Right (Report (Report.Audit _ _ _))) = \duration req -> Audit.entry Audit.Success Audit.RoutingRequest duration "REPORT-AUDIT" $ req

audit :: Tier3.Settings -> Either Error Route -> Number -> HTTP.IncomingMessage -> Aff Unit
audit settings result duration req = do
  entry <- pure $ routingRequest result duration req
  _ <- Tier3.execute $ Tier3.request settings (Tier3.InsertQuery $ Tier3.InsertAudit entry)
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
