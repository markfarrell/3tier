module Text.Parsing.Windows.Security
  ( event
  ) where

import Prelude

import Data.Traversable (sequence)

import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (choice)

import Foreign (Foreign)

import Data.Windows.Security as Security

import Text.Parsing.Common as Common
import Text.Parsing.Risk (injection) as Risk

message :: Foreign -> Parser String Security.Message
message = \x -> choice [subject x]

subject :: Foreign -> Parser String Security.Message
subject = \x -> do
  securityID    <- Common.validation  "securityID"    x $ Risk.injection
  accountName   <- Common.validation  "accountName"   x $ Risk.injection
  accountDomain <- Common.validation  "accountDomain" x $ Risk.injection
  logonID       <- Common.validation  "logonID"       x $ Risk.injection
  pure $ Security.Subject $
    { securityID    : securityID
    , accountName   : accountName
    , accountDomain : accountDomain
    , logonID       : logonID
    }

description :: Foreign -> Parser String (Array Security.Message)
description = \x -> do
  y <- Common.readArray "description" x
  z <- sequence (message <$> y)
  pure z

event :: Parser String Security.Event
event = do
  x                  <- Common.json
  eventID'           <- Common.property    "eventID"            x $ Common.nonnegativeInteger
  machineName        <- Common.validation  "machineName"        x $ Risk.injection
  entryNumber        <- Common.property    "entryNumber"        x $ Common.nonnegativeInteger
  entryData          <- Common.validation  "entryData"          x $ Risk.injection
  category           <- Common.validation  "category"           x $ Risk.injection
  categoryNumber     <- Common.property    "categoryNumber"     x $ Common.nonnegativeInteger
  entryType          <- Common.validation  "entryType"          x $ Risk.injection
  description'       <- description x
  source             <- Common.validation  "source"             x $ Risk.injection
  replacementStrings <- Common.validation  "replacementStrings" x $ Risk.injection
  instanceID         <- Common.validation  "instanceID"         x $ Risk.injection
  timeGenerated      <- Common.property    "timeGenerated"      x $ Common.date
  timeWritten        <- Common.property    "timeWritten"        x $ Common.date
  site               <- Common.validation  "site"               x $ Risk.injection
  container          <- Common.validation  "container"          x $ Risk.injection
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

