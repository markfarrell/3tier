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
  [ (string "/forward/alert?")   *> (Forward.Alert   <$> Alert.event)
  , (string "/forward/audit?")   *> (Forward.Audit   <$> Audit.event)
  , (string "/forward/traffic?") *> (Forward.Traffic <$> Traffic.event)
  , (string "/forward/linux?")   *> (Forward.Linux   <$> Linux.event)
  , (string "/forward/windows?") *> (Forward.Windows <$> Windows.event)
  ]
