module FFI.Date
  ( Date 
  , current
  , toISOString
  , getYear
  , getMilliseconds
  , getTime
  , currentTime
  , parse
  , epoch
  ) where

import Prelude

import Data.Either(Either(..))

import Effect (Effect)
import Effect.Exception (Error)
import Effect.Exception as Exception

foreign import data Date :: Type

foreign import current :: Effect Date

foreign import toISOString :: Date -> String

foreign import getYear :: Date -> Number

foreign import getMilliseconds :: Date -> Number

foreign import getTime :: Date -> Number

foreign import isValid :: String -> Boolean

foreign import parseImpl :: String -> Date

foreign import epoch :: Date

instance showDate :: Show Date where
  show = toISOString

instance eqDate :: Eq Date where
  eq x y = eq (toISOString x) (toISOString y) 

currentTime :: Effect Number
currentTime = getTime <$> current

parse :: String -> Either Error Date
parse dateString = case isValid dateString of
  false -> Left  $ Exception.error "Invalid date."
  true  -> Right $ parseImpl dateString
