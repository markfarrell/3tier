module Text.Parsing.Alert
  ( event
  ) where

import Text.Parsing.Parser (Parser)

import Data.Alert as Alert

import Data.Event as E

import Text.Parsing.Common (array)
import Text.Parsing.Event (event) as Event

event :: Parser String Alert.Event
event = Event.event (array E.eventCategories) (array E.eventTypes) (array E.eventIDs)
