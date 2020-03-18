module Assert
  ( assert
  ) where

import Prelude

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)

import Effect.Exception as Exception
  
assert :: forall a. Show a => Eq a => String -> a -> a -> Aff Unit
assert label expect check = do
  result    <- pure $ check == expect
  let entry = {  label : label, check : check, expect : expect }
  case result of
    true  -> do
      _ <- liftEffect $ log ("[SUCCESS] " <> label)
      pure unit
    false -> do 
      _ <- liftEffect $ log ("[FAILURE] " <> label)
      throw $ show entry
  where throw = liftEffect <<< Exception.throw 
