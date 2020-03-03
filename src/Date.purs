module Date
  ( Date 
  , current
  , toISOString
  , getMilliseconds
  , isValid
  ) where

import Effect (Effect)

foreign import data Date :: Type

foreign import current :: Effect Date

foreign import toISOString :: Date -> String

foreign import getMilliseconds :: Date -> Number

foreign import isValid :: String -> Boolean
