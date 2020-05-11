module Text.Parsing.Alert
  ( event
  ) where

import Text.Parsing.Parser (Parser)

import Data.Alert as Alert

import Data.Event as Event

import Text.Parsing.Common (array)
import Text.Parsing.Event (event) as E

event :: Parser String Alert.Event
event = E.event (array Event.eventCategories) (array Event.eventIDs)
