module Text.Parsing.Audit
  ( event
  ) where

import Text.Parsing.Parser (Parser)

import Data.Audit as Audit
import Text.Parsing.Event (event) as Event

event :: Parser String Audit.Event
event = Event.event Audit.eventCategories Audit.eventIDs
