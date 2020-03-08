module Buffer
  ( Buffer
  , from
  , readInt16BE
  , readInt32BE
  ) where

import Prelude

import Control.Monad.Error.Class (try)

import Data.Either (Either)

import Effect (Effect)
import Effect.Exception (Error)

foreign import data Buffer :: Type

foreign import from :: Array Int -> Effect Buffer

foreign import readInt16BEImpl :: Buffer -> Effect Int

foreign import readInt32BEImpl :: Buffer -> Effect Int

readInt16BE :: Buffer -> Effect (Either Error Int)
readInt16BE buffer = try $ readInt16BEImpl buffer

readInt32BE :: Buffer -> Effect (Either Error Int)
readInt32BE buffer = try $ readInt32BEImpl buffer
