module FFI.Math
  ( random
  , floor
  ) where

import Effect (Effect)

foreign import random :: Effect Number

foreign import floor :: Number -> Int
