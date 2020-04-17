module Data.IPv4
  ( IPv4(..)
  ) where

import Prelude

import Data.Foldable (intercalate)

data IPv4 = IPv4 Int Int Int Int

instance showIPv4 :: Show IPv4 where
  show (IPv4 w x y z) = intercalate dot (show <$> [w, x, y ,z])

instance eqIPv4 :: Eq IPv4 where
  eq (IPv4 w x y z) (IPv4 w' x' y' z') = eq [w,x,y,z] [w',x',y',z']

dot :: String
dot = "."
