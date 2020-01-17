module JSON
  ( stringify
  ) where

import Foreign (Foreign)

foreign import stringify :: Foreign -> String
