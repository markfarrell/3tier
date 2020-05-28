module Text.Parsing.Windows
  ( event
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))

import Text.Parsing.Parser (Parser, fail)

import Data.Event (Event(..))
import Data.Event   as E
import Data.Windows as Windows

import Text.Parsing.Common (array)
import Text.Parsing.Event (event) as Event

event :: Parser String Windows.Event
event = do
  x <- Event.event (array E.eventCategories) (array E.eventTypes) (array E.eventIDs) 
  case x of (Event y) ->
    case Array.elemIndex y.eventID (Windows.eventIDs' y.eventCategory) of
      (Just _)  -> pure x
      (Nothing) -> fail $ intercalate "" ["(", show y.eventID, ",", show y.eventCategory, ")"]
