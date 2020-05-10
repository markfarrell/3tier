module Text.Parsing.Forward 
  ( event
  ) where

import Prelude

import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.String (string)
import Text.Parsing.Parser.Combinators (choice)

import Data.Forward as Forward

import Text.Parsing.Audit as Audit

event :: Parser String Forward.URI
event = choice $
  [ (string "/forward/audit?") *> (Forward.Audit <$> Audit.event)
  ]
