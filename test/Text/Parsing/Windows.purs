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

import Data.Windows (EventType(..), EventURI(..)) as Windows

import Text.Parsing.Parser (runParser)

import Control.Forward (URI(..)) as Forward

import Effect.Windows (random) as Windows

import Text.Parsing.Forward (event)    as Forward
import Text.Parsing.Windows (eventURI) as Windows

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
      { eventID            : 5050
      , machineName        : "ABCD1234-XYZ.ca"
      , entryNumber        : 54669
      , entryData          : "System.Byte[]"
      , category           : "(13312)"
      , categoryNumber     : 13312
      , entryType          : Windows.Success
      , message            : ""
      , source             : "???" 
      , replacementStrings : "System.String[]"
      , instanceID         : "5050"
      , timeGenerated      : Date.epoch
      , timeWritten        : Date.epoch
      , site               : ""
      , container          : ""
      }

forward :: Effect Unit
forward = do
  input  <- Forward.Windows <$> Windows.random
  result <- pure (flip runParser Forward.event $ show input)
  case result of
    (Left  _)      -> Exception.throw (show result)
    (Right output) ->
      case input == output of
        false -> Exception.throw (show $ [input, output])
        true  -> pure unit

forwards :: Effect Unit
forwards = void $ sequence (const forward <$> Array.range 1 10)

suite :: Aff Unit
suite = do 
  _ <- liftEffect $ eventURI
  _ <- liftEffect $ forwards
  pure unit
