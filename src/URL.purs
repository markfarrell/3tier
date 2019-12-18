module URL
  ( runMatch
  ) where

import Prelude

import Control.Monad.Error.Class (try)

import Data.Either (Either)

import Effect (Effect)
import Effect.Exception (Error)

foreign import runMatchImpl :: String -> String -> String -> Effect String

runMatch :: String -> String -> String -> Effect (Either Error String)
runMatch pathname parameter path = try $ runMatchImpl pathname parameter path
