module Effect.Forward
  ( random
  ) where

import Prelude

import Effect (Effect)

import Data.Forward as Forward

import Effect.Range (random) as Range

import Effect.Alert   (random) as Alert
import Effect.Audit   (random) as Audit
import Effect.Traffic (random) as Traffic
import Effect.Linux   (random) as Linux
import Effect.Windows (random) as Windows

random :: Effect Forward.Event
random = do
  x <- Range.random 0 4
  case x of
    0 -> Forward.Alert   <$> Alert.random
    1 -> Forward.Audit   <$> Audit.random
    2 -> Forward.Traffic <$> Traffic.random
    3 -> Forward.Linux   <$> Linux.random
    _ -> Forward.Windows <$> Windows.random
