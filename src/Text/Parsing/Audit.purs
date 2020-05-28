module Text.Parsing.Audit
  ( event
  ) where

import Text.Parsing.Parser (Parser)

import Data.Audit as Audit

import Data.Event as E

import Text.Parsing.Common (array)
import Text.Parsing.Event (event) as Event

event :: Parser String Audit.Event
event = Event.event (array E.eventCategories) (array E.eventTypes) (array E.eventIDs)
