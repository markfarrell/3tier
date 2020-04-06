module Route
  ( Route(..)
  , execute
  , uri
  ) where

import Prelude

import Data.Either (Either(..))

import Effect.Aff (Aff)
import Effect.Exception (Error, error)

import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.String (string)
import Text.Parsing.Parser.Combinators (choice)

import Forward as Forward
import Data.Flow as Flow
import Report as Report

import FFI.HTTP as HTTP
import Strings as Strings

data Route = Forward Forward.Forward | Report Report.Report

report :: Report.Report -> Parser String Route
report report' = do
  _ <- string $ Report.uri report'
  pure $ (Report report')

reports :: Parser String Route
reports = choice (report <$> Report.all)

forwardFlow :: Parser String Route
forwardFlow = do
  _     <- string "/forward/flow?"
  event <- Flow.event
  pure (Forward (Forward.Flow event))

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

uri :: Route -> String
uri (Forward event)  = Forward.uri event
uri (Report  event)  = Report.uri event
