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

import Text.Parsing.Parser (runParser)

import Effect.Windows (random) as Windows

import Text.Parsing.Windows (event) as Windows

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
  _ <- liftEffect $ events
  pure unit
