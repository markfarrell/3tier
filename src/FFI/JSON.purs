module FFI.JSON
  ( stringify
  , parse
  ) where

import Data.Either (Either(..))

import Foreign (Foreign)

import Effect.Exception(Error)

foreign import stringify :: Foreign -> String

foreign import parseImpl :: (Error -> Either Error Foreign) -> (Foreign -> Either Error Foreign) -> String -> Either Error Foreign

parse :: String -> Either Error Foreign
parse = parseImpl Left Right
