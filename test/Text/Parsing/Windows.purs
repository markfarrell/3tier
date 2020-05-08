module Test.Text.Parsing.Windows
  ( suite
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Traversable (sequence)

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw) as Exception

import FFI.Date as Date

import Data.Windows (EventURI(..), EventURIComponent(..)) as Windows

import Text.Parsing.Parser (runParser)

import Effect.Windows (random) as Windows

import Text.Parsing.Windows (event,eventURI) as Windows

eventURI :: Effect Unit
eventURI = do
  result <- pure (flip runParser Windows.eventURI $ show input)
  case result of
    (Left  _)      -> Exception.throw (show result)
    (Right output) ->
      case input == output of
        false -> Exception.throw (show $ [input, output])
        true  -> pure unit
  where
    input = Windows.Security $
      { eventID            : 4625
      , machineName        : "ABCD1234-XYZ.ca"
      , entryNumber        : 54669
      , entryData          : "System.Byte[]"
      , category           : "(13312)"
      , categoryNumber     : 13312
      , entryType          : "SuccessAudit"
      , description        : [ Windows.Subject $
                               { securityID    : "S-1-5-18"
                               , accountName   : "ABCD1234-XY"
                               , accountDomain : "ABCD1234"
                               , logonID       : "0x3e7"
                               }
                             ]
      , source             : "Microsoft-Windows-Security-Auditing" 
      , replacementStrings : "System.String[]"
      , instanceID         : "4625"
      , timeGenerated      : Date.epoch
      , timeWritten        : Date.epoch
      , site               : ""
      , container          : ""
      }

event :: Effect Unit
event = do
  input  <- Windows.random
  result <- pure (flip runParser Windows.event $ show input)
  case result of
    (Left  _)      -> Exception.throw (show result)
    (Right output) ->
      case input == output of
        false -> Exception.throw (show $ [input, output])
        true  -> pure unit

events :: Effect Unit
events = void $ sequence (const event <$> Array.range 1 10)

suite :: Aff Unit
suite = do 
  _ <- liftEffect $ eventURI
  _ <- liftEffect $ events
  pure unit
