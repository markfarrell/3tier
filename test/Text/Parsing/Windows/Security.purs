module Test.Text.Parsing.Windows.Security
  ( suite
  ) where

import Prelude

import Data.Either (Either(..))

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw) as Exception

import FFI.Date as Date

import Data.Windows.Security (Event(..), Message(..)) as Security

import Text.Parsing.Parser (runParser)

import Text.Parsing.Windows.Security (event) as Security

event :: Effect Unit
event = do
  result <- pure (flip runParser Security.event $ show input)
  case result of
    (Left  _)      -> Exception.throw (show result)
    (Right output) ->
      case input == output of
        false -> Exception.throw (show $ [input, output])
        true  -> pure unit
  where
    input = Security.Event $
      { eventID            : 4625
      , machineName        : "ABCD1234-XYZ.ca"
      , entryNumber        : 54669
      , entryData          : "System.Byte[]"
      , category           : "(13312)"
      , categoryNumber     : 13312
      , entryType          : "SuccessAudit"
      , description        : [ subject ]
      , source             : "Microsoft-Security-Security-Auditing" 
      , replacementStrings : "System.String[]"
      , instanceID         : "4625"
      , timeGenerated      : Date.epoch
      , timeWritten        : Date.epoch
      , site               : ""
      , container          : ""
      }
    subject = Security.Subject $
      { securityID    : "S-1-5-18"
      , accountName   : "ABCD1234-XY"
      , accountDomain : "ABCD1234"
      , logonID       : "0x3e7"
      }

suite :: Aff Unit
suite = do 
  _ <- liftEffect $ event
  pure unit
