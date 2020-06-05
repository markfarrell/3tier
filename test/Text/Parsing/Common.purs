module Test.Text.Parsing.Common
  ( suite
  ) where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

import Text.Parsing.Common as C

import Text.Parsing.Expect as E

uuid :: Effect Unit
uuid = do
  _ <- failure ""
  _ <- failure "82fa951b"
  _ <- failure "82fa951b-b591"
  _ <- failure "82fa951b-b591-4ace"
  _ <- failure "82fa951b-b591-4ace-b473"
  _ <- failure "82fa951b-b591-4ace-b473-f2aacaed56f"
  _ <- success "82fa951b-b591-4ace-b473-f2aacaed56fd"
  pure unit
  where
    success = \check -> E.success check (C.uuid)
    failure = \check -> E.failure check (C.uuid)
    output  = \check expect -> E.output check expect (show <$> C.uuid)

suite :: Aff Unit
suite = liftEffect $ do
  _ <- uuid
  pure unit
