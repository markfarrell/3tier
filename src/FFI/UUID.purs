module FFI.UUID
  ( UUID
  , uuidv1
  , uuidv4
  , uuidv5
  ) where

import Prelude

import Effect (Effect)

import Unsafe.Coerce (unsafeCoerce)

foreign import data UUID :: Type

foreign import uuidv1 :: Effect UUID

foreign import uuidv4 :: Effect UUID

foreign import uuidv5 :: String -> UUID -> Effect UUID

instance showUUID :: Show UUID where
  show = unsafeCoerce

instance eqUUID :: Eq UUID where
  eq x y = (show x) == (show y)
