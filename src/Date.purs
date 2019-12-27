module Date
  ( Date 
  , current
  , toISOString
  ) where

import Effect (Effect)

foreign import data Date :: Type

foreign import current :: Effect Date

foreign import toISOString :: Date -> String
