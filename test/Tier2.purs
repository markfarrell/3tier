module Test.Tier2 where

import Prelude

import Data.Either(Either)

import Effect (Effect)
import Effect.Aff (Aff, launchAff, forkAff, killFiber)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error)

import HTTP as HTTP

import Flow as Flow

import Tier2 as Tier2
import Tier3 as Tier3

forwardFlow :: Tier3.Settings -> Tier2.Settings -> Flow.Entry -> Aff (Either Error HTTP.IncomingResponse)
forwardFlow settings (Tier2.Settings tier2) entry = do
  server <- liftEffect (HTTP.createServer)
  fiber  <- forkAff $ Tier2.start settings server
  _      <- liftEffect (HTTP.listen tier2.port $ server)
  result <- Tier2.forwardFlow (Tier2.Settings tier2) entry
  _      <- flip killFiber fiber $ error "Expected behaviour."
  _      <- liftEffect (HTTP.close server)
  pure result

main :: Effect Unit
main = void $ launchAff $ do
  pure unit
