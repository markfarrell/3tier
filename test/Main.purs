module Test.Main where

import Prelude

import Data.Either (isRight)

import Effect (Effect)
import Effect.Aff (Aff, launchAff)

import Assert (assert)

import DB as DB

testRequest :: forall a. DB.Request a -> Aff Unit
testRequest request = do
  result <- DB.runRequest request
  assert true $ isRight result

main :: Effect Unit
main = void $ launchAff $ do
  _ <- testRequest $ DB.touch ":memory:"
  pure unit
