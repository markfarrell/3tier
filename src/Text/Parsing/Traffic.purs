module Text.Parsing.Traffic
  ( event
  ) where

import Text.Parsing.Parser (Parser)

import Data.Traffic as Traffic

import Text.Parsing.Common (array, port)
import Text.Parsing.Event (event) as Event

event :: Parser String Traffic.Event
event = Event.event (array Traffic.eventCategories) (port) 
