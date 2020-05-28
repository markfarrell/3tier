module Text.Parsing.Traffic
  ( event
  ) where

import Prelude

import Text.Parsing.Parser (Parser)

import Data.Traffic as Traffic

import Data.Event as E

import Text.Parsing.Common (array, port)
import Text.Parsing.Event (event) as Event

event :: Parser String Traffic.Event
event = Event.event (array E.eventCategories) (Traffic.EventID <$> port) 
