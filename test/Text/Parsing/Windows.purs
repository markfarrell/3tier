module Test.Text.Parsing.Windows
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

import Effect.Windows (random) as Windows

import Text.Parsing.Forward (event) as Forward

forward :: Effect Unit
forward = do
  input  <- Forward.Windows <$> Windows.random
  _      <- log (show input)
  result <- pure (flip runParser Forward.event $ show input)
  case result of
    (Left  _)      -> Exception.throw (show result) 
    (Right output) ->
      case input == output of
        false -> Exception.throw (intercalate " " ["[FAILURE]", "CHECK/EXPECT", show input, show output]) 
        true  -> log $ intercalate " " ["[TEST]", show output]

forwards :: Effect Unit
forwards = void $ sequence (const forward <$> Array.range 1 10)

main :: Effect Unit
main = do
  _ <- forwards
  pure unit
