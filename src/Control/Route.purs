  module Control.Route
  ( Route(..)
  , execute
  ) where

import Prelude

import Data.Either (Either(..))

import Effect.Aff (Aff)
import Effect.Exception (Error, error)

import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.String (string)
import Text.Parsing.Parser.Combinators (choice)

import Text.Parsing.Flow (event) as Flow

import Control.Forward as Forward
import Control.Report as Report

import FFI.HTTP as HTTP
import FFI.String as String

data Route = Forward Forward.URI | Report Report.URI

instance showRouteTier3 :: Show Route where
  show = uri

report :: Report.URI -> Parser String Route
report report' = do
  _ <- string $ Report.uri report'
  pure $ (Report report')

reports :: Parser String Route
reports = choice (report <$> Report.uris)

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
  result <- pure (flip runParser route $ String.decodeURIComponent (HTTP.messageURL req)) 
  case result of
    (Left _)        -> pure (Left $ error "Invalid routing request.")
    (Right result') -> pure (Right $ result')

uri :: Route -> String
uri (Forward event)  = Forward.uri event
uri (Report  event)  = Report.uri event
