module Main
  ( main
  ) where

import Prelude

import Control.Coroutine as Coroutine

import Effect (Effect)
import Effect.Aff (forkAff, supervise, launchAff)
import Effect.Class (liftEffect)

import FFI.HTTP as HTTP

import Control.Tier2 as Tier2

main :: Effect Unit
main = void $ launchAff $ supervise $ do
  server <- liftEffect $ HTTP.createServer 
  _      <- forkAff (Coroutine.runProcess $ Tier2.process server)
  _      <- liftEffect $ HTTP.listen port $ server 
  pure unit
  where port = 3000
