module Foreign.Coerce
  ( class Marshall
  , marshall
  , coerce
  ) where

import Foreign (Foreign)

import Unsafe.Coerce (unsafeCoerce)

class Marshall a where
  marshall :: a -> Foreign

coerce :: forall a. a -> Foreign
coerce = unsafeCoerce
