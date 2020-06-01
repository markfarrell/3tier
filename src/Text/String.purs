module Text.String
  ( fromArray
  , fromChar
  ) where

import Data.Foldable (foldMap)

import Data.String.CodeUnits (singleton)

{-- | Converts an array of characters to a string. --}
fromArray :: Array Char -> String
fromArray = foldMap singleton

fromChar :: Char -> String
fromChar = singleton
