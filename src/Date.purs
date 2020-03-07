module Date
  ( Date 
  , current
  , toISOString
  , getMilliseconds
  , getTime
  , isValid
  , currentTime
  ) where

import Prelude

import Effect (Effect)

foreign import data Date :: Type

foreign import current :: Effect Date

foreign import toISOString :: Date -> String

foreign import getMilliseconds :: Date -> Number

foreign import getTime :: Date -> Number

foreign import isValid :: String -> Boolean

currentTime :: Effect Number
currentTime = getTime <$> current
