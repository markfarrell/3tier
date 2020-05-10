module Text.Parsing.Linux
  ( event
  ) where

import Text.Parsing.Parser (Parser)

import Data.Linux as Linux

import Text.Parsing.Common (array, nonnegativeInteger)
import Text.Parsing.Event (event) as Event

event :: Parser String Linux.Event
event = Event.event (array Linux.eventCategories) (nonnegativeInteger) 
