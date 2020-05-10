module Effect.Port
  ( random
  ) where

import Prelude

import Effect (Effect)

import Unsafe.Coerce (unsafeCoerce)

import Data.Port (Port)

import Effect.Range as Range

random :: Effect Port
random = unsafeCoerce <$> Range.random 0 65535
