module Foreign.Object
  ( fromArray
  ) where

import Data.Tuple (Tuple, fst, snd)

import Foreign (Foreign)

foreign import fromArrayImpl :: (Tuple String Foreign -> String) -> (Tuple String Foreign -> Foreign) -> Array (Tuple String Foreign) -> Foreign 

fromArray :: Array (Tuple String Foreign) -> Foreign
fromArray = fromArrayImpl fst snd
