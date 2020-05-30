module Text.Parsing.Alphanumeric
  ( lowercase
  ) where

import Prelude

import Data.Array as Array
import Data.List as List

import Data.Foldable (foldMap)

import Data.String.CodeUnits (singleton)

import Text.Parsing.Parser (Parser, fail)

import Text.Parsing.Parser.Combinators as C
import Text.Parsing.Parser.String as S

{-- | Parses a single lowercase alphanumeric character. --}
lowercaseChar :: Parser String Char
lowercaseChar = C.choice (S.char <$> valid) 
  where 
    valid   = letters <> digits
    letters = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h']
      <> ['i', 'j', 'k', 'l', 'm', 'n', 'o', 'p'] 
      <> ['q', 'r', 's', 't', 'u', 'v', 'w', 'x']
      <> ['y', 'z']
    digits  = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

{-- | Parses a lowercase alphanumeric string. --}
lowercase :: Parser String String
lowercase = do
  x <- Array.fromFoldable <$> List.many lowercaseChar 
  case (Array.length x) > 0 of
    false -> fail "Invalid # of lowercase characters."
    true  -> pure $ foldMap singleton x
