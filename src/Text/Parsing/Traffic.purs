module Text.Parsing.Traffic
  ( event
  ) where

import Prelude

import Text.Parsing.Parser (Parser)

import Data.Traffic as Traffic

import Data.Event as Event

import Text.Parsing.Common (array, port)
import Text.Parsing.Event (event) as E

event :: Parser String Traffic.Event
event = E.event (array Event.eventCategories) (Traffic.EventID <$> port) 
