module Test.Text.Parsing
  ( assert
  ) where

import Prelude

import Data.Either as E

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw) as Exception

import Text.Parsing.Parser (Parser, runParser)

assert :: forall a b. Boolean -> a -> Parser a b -> Aff Unit
assert expect check parser = do
  result <- pure $ runParser check parser
  _      <- assert' expect (E.isRight result)
  pure unit

assert' :: Boolean -> Boolean -> Aff Unit
assert' x y = case x == y of
  false -> liftEffect $ Exception.throw ("Test.Text.Parsing.assert (unexpected result).")
  true  -> pure unit
