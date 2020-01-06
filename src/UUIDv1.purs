module UUIDv1
  ( createUUID
  ) where

import Effect (Effect)

foreign import createUUID :: Effect String
