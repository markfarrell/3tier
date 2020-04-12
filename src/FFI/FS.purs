module FFI.FS
  (unlink
  ) where

import Prelude

import Control.Monad.Error.Class (try)

import Data.Either (Either)

import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Exception (Error)

foreign import unlinkImpl :: String -> EffectFnAff Unit

unlink :: String -> Aff (Either Error Unit)
unlink path = try $ fromEffectFnAff (unlinkImpl path)
