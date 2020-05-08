module Test.Text.Parsing.Alert
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

import Text.Parsing.Parser (runParser)

import Effect.Alert (random) as Alert

import Text.Parsing.Alert (event) as Alert

event :: Effect Unit
event = do
  input  <- Alert.random
  result <- pure (flip runParser Alert.event $ show input)
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
  _ <- liftEffect $ events
  pure unit
