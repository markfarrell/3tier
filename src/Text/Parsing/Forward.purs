module Text.Parsing.Forward 
  ( event
  ) where

import Prelude

import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.String (string)
import Text.Parsing.Parser.Combinators (choice)

import Data.Forward as Forward

import Text.Parsing.Alert   as Alert
import Text.Parsing.Audit   as Audit
import Text.Parsing.Traffic as Traffic
import Text.Parsing.Linux   as Linux
import Text.Parsing.Windows as Windows

event :: Parser String Forward.Event
event = choice $
  [ (string "/forward/alert?event=")   *> (Forward.Alert <$> Alert.event)
  , (string "/forward/audit?event=")   *> (Forward.Audit <$> Audit.event)
  , (string "/forward/traffic?event=") *> (Forward.Traffic <$> Traffic.event)
  , (string "/forward/linux?event=")   *> (Forward.Linux <$> Linux.event)
  , (string "/forward/windows?event=") *> (Forward.Windows <$> Windows.event)
  ]
