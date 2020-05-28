module Text.Parsing.Linux
  ( event
  ) where

import Prelude

import Text.Parsing.Parser (Parser)

import Data.Linux as Linux

import Data.Event as E

import Text.Parsing.Common (array, nonnegativeInteger)
import Text.Parsing.Event (event) as Event

event :: Parser String Linux.Event
event = Event.event (array E.eventCategories) (array E.eventTypes) (Linux.EventID <$> nonnegativeInteger) 
