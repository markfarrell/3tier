module Assert
  ( assert
  ) where

import Prelude

import Effect.Aff (Aff)
import Effect.Class (liftEffect)

import Effect.Exception as Exception

import Date as Date

import Audit as Audit
  
assert :: forall a. Show a => Eq a => a -> a -> Aff Unit
assert expect check = do
  startTime <- liftEffect (Date.getMilliseconds <$> Date.current)
  result    <- pure $ check == expect
  endTime   <- liftEffect (Date.getMilliseconds <$> Date.current)
  let entry = {  check : check, expect : expect, delta : (endTime - startTime) }
  case result of
    true  -> do
      _ <- Audit.debug $ Audit.Entry Audit.Success Audit.AssertRequest (show entry)
      pure unit
    false -> do
      _ <- Audit.debug $ Audit.Entry Audit.Failure Audit.AssertRequest (show entry)
      throw $ show entry
  where throw = liftEffect <<< Exception.throw 
