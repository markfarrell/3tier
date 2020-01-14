module Arrays
  ( join
  ) where

import Prelude (class Monoid, mempty, (<>))

import Data.Array as Array
import Data.Maybe (Maybe(..))

import Data.Foldable (foldl)

join :: forall a. Monoid a => a -> Array a -> a
join  separator array = join' separator (Array.head array) (Array.tail array)
  where 
    join' separator' (Just a) (Just b)   = foldl (\x y -> x <> separator' <> y) a b
    join' separator' (Just a) (Nothing)  = a
    join' separator' (Nothing) _         = mempty
