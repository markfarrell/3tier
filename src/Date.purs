module Date
  ( now
  ) where

import Effect (Effect)

foreign import now :: Effect Number
