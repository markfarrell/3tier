module Data.Port
  ( Port
  ) where

import Prelude

import Unsafe.Coerce (unsafeCoerce)

foreign import data Port :: Type

port :: Port -> Int
port = unsafeCoerce

instance showPort :: Show Port where
  show = show <<< port

instance eqPort :: Eq Port where
  eq x y = eq (port x) (port y)
