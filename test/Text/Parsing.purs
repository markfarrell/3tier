module Test.Text.Parsing
  ( main
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Traversable (sequence)

import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw) as Exception

import Text.Parsing.Parser (runParser)

import Effect.Flow (random) as Flow
import Text.Parsing.Flow (event) as Flow

flowEvent :: Effect Unit
flowEvent = do
  input  <- Flow.random
  result <- pure (flip runParser Flow.event $ show input)
  case result of
    (Left  _)      -> Exception.throw (show result) 
    (Right output) ->
      case input == output of
        false -> Exception.throw (show [input,output]) 
        true  -> log $ intercalate " " ["[TEST]", show input, "==", show output]

flowEvents :: Effect Unit
flowEvents = void $ sequence (const flowEvent <$> Array.range 1 10)

main :: Effect Unit
main = do
  _ <- flowEvents
  pure unit
