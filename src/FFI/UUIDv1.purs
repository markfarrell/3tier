module FFI.UUIDv1
  ( createUUID
  , defaultUUID
  ) where

import Effect (Effect)

foreign import createUUID :: Effect String

foreign import defaultUUID :: String
