module Test.Text.Parsing.Common
  ( suite
  ) where

import Prelude

import Data.Either (Either(..))

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw) as Exception

import Text.Parsing.Parser (runParser)

import Text.Parsing.Common (substring)

suite :: Aff Unit
suite = do
  _ <- runParser' "' OR 1=1" (substring "")
  _ <- runParser' "' OR 1=1" (substring "OR")
  _ <- runParser' "' OR 1=1" (substring "' OR 1=1")
  pure unit
  where
    runParser' x y = do
      z <- pure $ runParser x y
      case z of
        (Left  _)  -> liftEffect $ Exception.throw ("Test.Text.Parsing.Common (" <> x <> ")")
        (Right z') -> pure z' 
