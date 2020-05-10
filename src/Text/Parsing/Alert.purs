module Text.Parsing.Alert
  ( event
  ) where

import Text.Parsing.Parser (Parser)

import Data.Alert as Alert

import Text.Parsing.Common (array)
import Text.Parsing.Event (event) as Event

event :: Parser String Alert.Event
event = Event.event (array Alert.eventCategories) (array Alert.eventIDs)
