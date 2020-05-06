module Test.Control.Tier3
  ( availability
  ) where

import Prelude

import Effect.Aff (Aff)

import Test.Control.Tier3.Availability as Availability

availability :: Aff Unit
availability = Availability.suite
