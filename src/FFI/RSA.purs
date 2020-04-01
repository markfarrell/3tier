module FFI.RSA
  ( Key
  , generateKeyPair
  , sign
  , verify 
  ) where

import Effect (Effect)

foreign import data Key :: Type

foreign import generateKeyPair :: Effect Key

foreign import sign :: Key -> String -> String

foreign import verify :: Key -> String -> String -> Boolean
