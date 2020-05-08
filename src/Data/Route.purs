module Data.Route
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

import Data.Forward (URI) as Forward
import Data.Report as Report

import FFI.HTTPS as HTTPS
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

execute :: HTTPS.IncomingMessage -> Aff (Either Error Route)
execute req = do 
  result <- pure (flip runParser route $ String.decodeURIComponent (HTTPS.messageURL req)) 
  case result of
    (Left _)        -> pure (Left $ error "Invalid routing request.")
    (Right result') -> pure (Right $ result')
