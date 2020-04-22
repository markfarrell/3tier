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

import Text.Parsing.Forward (event) as Forward

import Control.Forward (URI) as Forward
import Control.Report as Report

import FFI.HTTP as HTTP
import FFI.String as String

data Route = Forward Forward.URI | Report Report.URI

instance showRoute :: Show Route where
  show (Forward uri)  = show uri
  show (Report  uri)  = show uri

report :: Report.URI -> Parser String Route
report report' = (string $ show report') *> pure (Report report')

reports :: Parser String Route
reports = choice (report <$> Report.uris)

route :: Parser String Route
route = choice [Forward <$> Forward.event, reports]

execute :: HTTP.IncomingMessage -> Aff (Either Error Route)
execute req = do 
  result <- pure (flip runParser route $ String.decodeURIComponent (HTTP.messageURL req)) 
  case result of
    (Left _)        -> pure (Left $ error "Invalid routing request.")
    (Right result') -> pure (Right $ result')
