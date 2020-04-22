module Text.Parsing.Forward
  ( event
  ) where

import Prelude

import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.String (string)
import Text.Parsing.Parser.Combinators (choice)

import Control.Forward as Forward

import Text.Parsing.Flow as Flow
import Text.Parsing.Windows as Windows

event :: Parser String Forward.URI
event = choice $
  [ (string "/forward/flow?")    *> (Forward.Flow <$> Flow.event)
  , (string "/forward/windows?") *> (Forward.Windows <$> Windows.event)
  ]
