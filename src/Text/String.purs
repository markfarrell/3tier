module Text.String
  ( fromArray
  ) where

import Data.Foldable (foldMap)

import Data.String.CodeUnits (singleton)

{-- | Converts an array of characters to a string. --}
fromArray :: Array Char -> String
fromArray = foldMap singleton
