module Test.Text.Parsing
  ( assert
  ) where

import Prelude

import Data.Either as E

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw) as Exception

import Text.Parsing.Parser (Parser, runParser)

assert :: forall a b. Show a => Show b => Boolean -> a -> Parser a b -> Aff Unit
assert expect check parser = do
  result <- pure $ runParser check parser
  _      <- assert' check result expect (E.isRight result)
  pure unit

assert' :: forall a b. Show a => Show b => a -> b -> Boolean -> Boolean -> Aff Unit
assert' w x y z = case y == z of
  false -> liftEffect $ Exception.throw ("Test.Text.Parsing.assert (" <> show w <> "," <> show x <> "," <> show y <> "," <> show z <> ")")
  true  -> pure unit
