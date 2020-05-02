module Test.Text.Parsing.Flow
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

import Control.Forward (URI(..)) as Forward

import Effect.Flow (random) as Flow

import Text.Parsing.Forward (event) as Forward

forward :: Effect Unit
forward = do
  input  <- Forward.Flow <$> Flow.random
  result <- pure (flip runParser Forward.event $ show input)
  case result of
    (Left  _)      -> Exception.throw (show result) 
    (Right output) ->
      case input == output of
        false -> Exception.throw (show [input,output]) 
        true  -> pure unit

forwards :: Effect Unit
forwards = void $ sequence (const forward <$> Array.range 1 10)

suite :: Aff Unit
suite = liftEffect $ forwards
