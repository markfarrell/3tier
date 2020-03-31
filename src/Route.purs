module Route
  ( Route(..)
  , execute
  , eventID
  ) where

import Prelude

import Data.Either (Either(..))

import Effect.Aff (Aff)
import Effect.Exception (Error, error)

import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.String (string)
import Text.Parsing.Parser.Combinators (choice)

import Audit as Audit
import Forward as Forward
import Flow as Flow
import Report as Report

import HTTP as HTTP
import Strings as Strings

data Route = Forward Forward.Forward | Report Report.Report

eventID :: Route -> Audit.EventID
eventID (Forward (Forward.Audit _))    = [Audit.Forward Audit.Audit]
eventID (Forward (Forward.Flow _))     = [Audit.Forward Audit.Flow]
eventID (Report (Report.Audit _ _ _))  = [Audit.Report Audit.Audit]

reportAudit'' :: Audit.EventCategory -> String
reportAudit'' Audit.DatabaseRequest = "database-request"
reportAudit'' Audit.ResourceRequest = "resource-request"
reportAudit'' Audit.RoutingRequest  = "routing-request"

report :: Report.Report -> Parser String Route
report report' = do
  _ <- string $ Report.uri report'
  pure $ (Report report')

reports :: Parser String Route
reports = choice (report <$> Report.reports)

forwardFlow :: Parser String Route
forwardFlow = do
  _     <- string "/forward/flow/"
  entry <- Flow.flow
  pure (Forward (Forward.Flow entry))

forward :: Parser String Route
forward = choice [forwardFlow] 

route :: Parser String Route
route = choice [forward, reports]

execute :: HTTP.IncomingMessage -> Aff (Either Error Route)
execute req = do 
  result <- pure (flip runParser route $ Strings.decodeURIComponent (HTTP.messageURL req)) 
  case result of
    (Left _)        -> pure (Left $ error "Invalid routing request.")
    (Right result') -> pure (Right $ result')
