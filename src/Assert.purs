module Assert
  ( assert
  ) where

import Prelude

import Effect.Aff (Aff)
import Effect.Class (liftEffect)

import Effect.Exception as Exception

import Audit as Audit
  
assert :: forall a. Show a => Eq a => String -> a -> a -> Aff Unit
assert label expect check = do
  result    <- pure $ check == expect
  let entry = {  label : label, check : check, expect : expect }
  case result of
    true  -> do
      _ <- Audit.debug $ Audit.Entry Audit.Success Audit.AssertRequest (show entry)
      pure unit
    false -> do
      _ <- Audit.debug $ Audit.Entry Audit.Failure Audit.AssertRequest (show entry)
      throw $ show entry
  where throw = liftEffect <<< Exception.throw 
