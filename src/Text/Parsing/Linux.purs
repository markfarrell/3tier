module Text.Parsing.Linux
  ( event
  ) where

import Prelude

import Text.Parsing.Parser (Parser)

import Data.Linux as Linux

import Data.Event as Event

import Text.Parsing.Common (array, nonnegativeInteger)
import Text.Parsing.Event (event) as E

event :: Parser String Linux.Event
event = E.event (array Event.eventCategories) (Linux.EventID <$> nonnegativeInteger) 
