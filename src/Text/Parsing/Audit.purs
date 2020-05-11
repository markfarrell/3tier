module Text.Parsing.Audit
  ( event
  ) where

import Text.Parsing.Parser (Parser)

import Data.Audit as Audit

import Data.Event as Event

import Text.Parsing.Common (array)
import Text.Parsing.Event (event) as E

event :: Parser String Audit.Event
event = E.event (array Event.eventCategories) (array Event.eventIDs)
