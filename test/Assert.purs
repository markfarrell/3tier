module Test.Assert
  ( assert
  ) where

import Prelude

import Data.Either (Either(..))

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)

import Effect.Exception as Exception
  
assert' :: forall a b. String -> Either a b -> Effect Unit
assert' label (Left _)  = do 
  _ <- log ("[FAILURE] " <> label)
  _ <- Exception.throw label 
  pure unit
assert' label (Right _) = do 
  _ <- log ("[SUCCESS] " <> label)
  pure unit

assert :: forall a b. String -> Either a b -> Aff Unit
assert label result = liftEffect $ assert' label result
