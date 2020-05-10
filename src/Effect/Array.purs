module Effect.Array
  ( random
  ) where

import Prelude

import Data.Array as Array

import Data.Maybe (Maybe(..))

import Effect (Effect)

import Effect.Range as Range

index :: forall a. Array a -> Effect Int
index w = Range.random 0 ((Array.length w) - 1)

array :: forall a. Array a -> Effect (Maybe a)
array w = do
  x <- index w
  y <- pure $ Array.index w x
  pure y

random ::forall a. a -> Array a -> Effect a
random w x = do
  y <- array x
  case y of
    (Just z)  -> pure z
    (Nothing) -> pure w
