module Test.Text.Parsing.Risk
  ( suite
  ) where

import Prelude

import Data.Either (isLeft, isRight) as Either
import Data.Traversable (sequence)

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw) as Exception

import Text.Parsing.Parser (runParser)

import Text.Parsing.Risk as Risk

suite :: Aff Unit
suite = do
  _ <- sequence (checkSuccess <$> expectSuccess)
  _ <- sequence (checkFailure <$> expectFailure)
  pure unit
  where
    expectSuccess = ["'OR 1=1 --", "'OR 1=1', 'OR 1=1;'", "'; CREATE TABLE x () --'"]
    expectFailure = ["'OR 1=1`", "SELECT", "INSERT", "CREATE", "DROP", "DELETE", "UPDATE", "''", "'--", "'\"", "';"]
    checkSuccess  = flip runParser'  Risk.injection
    checkFailure  = flip runParser'' Risk.injection
    runParser' x y = do
      z <- pure $ runParser x y
      case Either.isLeft z of
        true  -> liftEffect $ Exception.throw ("Test.Text.Parsing.Risk (" <> x <> ")")
        false -> pure unit 
    runParser'' x y = do
      z <- pure $ runParser x y
      case Either.isRight z of
        true  -> liftEffect $ Exception.throw ("Test.Text.Parsing.Risk (" <> x <> ")")
        false -> pure unit 
