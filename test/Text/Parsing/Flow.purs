module Test.Text.Parsing.Flow
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
        true  -> log $ intercalate " " ["[TEST]", show output]

forwards :: Effect Unit
forwards = void $ sequence (const forward <$> Array.range 1 10)

main :: Effect Unit
main = do
  _ <- forwards
  pure unit
