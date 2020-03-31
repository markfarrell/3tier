module IPv4
  ( IPv4(..)
  ) where

import Prelude

import Arrays as Arrays

data IPv4 = IPv4 Int Int Int Int

instance showIPv4 :: Show IPv4 where
  show (IPv4 w x y z) = Arrays.join dot (show <$> [w, x, y ,z])

dot :: String
dot = "."
