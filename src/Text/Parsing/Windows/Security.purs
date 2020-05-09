module Text.Parsing.Windows.Security
  ( event
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))

import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.Combinators (choice)

import Foreign (Foreign)

import Data.Windows.Security as Security

import Text.Parsing.Common (date, json, nonnegativeInteger, property, validation , showable, readArray, readIndex)
import Text.Parsing.Event (source, time) as Event
import Text.Parsing.Risk (injection) as Risk

message :: Foreign -> Parser String Security.Message
message = \x -> choice [subject x]

subject :: Foreign -> Parser String Security.Message
subject = \x -> do
  securityID    <- validation  "securityID"    x $ Risk.injection
  accountName   <- validation  "accountName"   x $ Risk.injection
  accountDomain <- validation  "accountDomain" x $ Risk.injection
  logonID       <- validation  "logonID"       x $ Risk.injection
  pure $ Security.Subject $
    { securityID    : securityID
    , accountName   : accountName
    , accountDomain : accountDomain
    , logonID       : logonID
    }

description :: Foreign -> Parser String (Array Security.Message)
description = \x -> do
  y <- readArray "description" x
  z <- sequence (message <$> y)
  pure z

event :: Parser String Security.Event
event = do
  x                  <- json
  eventID'           <- property    "eventID"            x $ nonnegativeInteger
  machineName        <- validation  "machineName"        x $ Risk.injection
  entryNumber        <- property    "entryNumber"        x $ nonnegativeInteger
  entryData          <- validation  "entryData"          x $ Risk.injection
  category           <- validation  "category"           x $ Risk.injection
  categoryNumber     <- property    "categoryNumber"     x $ nonnegativeInteger
  entryType          <- validation  "entryType"          x $ Risk.injection
  description'       <- description x
  source             <- validation  "source"             x $ Risk.injection

  replacementStrings <- validation  "replacementStrings" x $ Risk.injection
  instanceID         <- validation  "instanceID"         x $ Risk.injection
  timeGenerated      <- property    "timeGenerated"      x $ date
  timeWritten        <- property    "timeWritten"        x $ date
  site               <- validation  "site"               x $ Risk.injection
  container          <- validation  "container"          x $ Risk.injection
  pure $ Security.Event $
    { eventID            : eventID'
    , machineName        : machineName
    , entryNumber        : entryNumber
    , entryData          : entryData
    , category           : category
    , categoryNumber     : categoryNumber
    , entryType          : entryType
    , description        : description'
    , source             : source
    , replacementStrings : replacementStrings
    , instanceID         : instanceID
    , timeGenerated      : timeGenerated
    , timeWritten        : timeWritten
    , site               : site
    , container          : container
    }

