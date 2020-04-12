module Test.Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff)

import Test.Control.Tier3 as Tier3

main :: Effect Unit
main = void $ launchAff $ do
  _ <- Tier3.suite
  pure unit
