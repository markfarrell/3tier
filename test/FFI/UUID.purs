module Test.FFI.UUID
  ( suite
  ) where

import Prelude

import Data.Time.Duration (Milliseconds(..))

import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Exception (throw) as Exception

import FFI.UUID as UUID

testUUIDv1 :: Aff Unit
testUUIDv1 = do
  x <- liftEffect $ UUID.uuidv1
  _ <- Aff.delay $ Milliseconds 1.0
  y <- liftEffect $ UUID.uuidv1
  case x == y of
    true  -> liftEffect $ Exception.throw $ show [x,y] 
    false -> pure unit

testUUIDv5 :: Aff Unit
testUUIDv5 = do
  w <- liftEffect $ UUID.uuidv1
  _ <- Aff.delay $ Milliseconds 1.0
  x  <- liftEffect $ UUID.uuidv1
  y  <- liftEffect $ UUID.uuidv5 input w
  y' <- liftEffect $ UUID.uuidv5 input w
  z  <- liftEffect $ UUID.uuidv5 input x
  case (not $ y == y') || (y == z) of
    true  -> liftEffect $ Exception.throw $ show [x,y] 
    false -> pure unit
  where input = "0.0.0.0:0"

suite :: Aff Unit
suite = do
  _ <- testUUIDv1
  _ <- testUUIDv5
  pure unit
