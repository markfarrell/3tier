module Text.Parsing.Forward
  ( event
  ) where

import Prelude

import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.String (string)
import Text.Parsing.Parser.Combinators (choice)

import Control.Forward as Forward

import Text.Parsing.Flow as Flow

forwardFlow :: Parser String Forward.URI
forwardFlow = (string "/forward/flow?") *> (Forward.Flow <$> Flow.event)

event :: Parser String Forward.URI
event = choice [forwardFlow] 
