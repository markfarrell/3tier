module Text.Parsing.Linux.Audit
  ( body
  ) where

import Prelude

import Data.Array as Array
import Data.List as List

import Data.Tuple (Tuple(..))

import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators as C
import Text.Parsing.Parser.String as S

import Foreign (Foreign)
import Foreign.Class (marshall)

import Text.Parsing.Alphanumeric as Alphanumeric
import Text.Parsing.Char as Char
import Text.Parsing.Repeat as Repeat

import Text.String as String

field :: Parser String (Tuple String Foreign)
field = do
  x <- Alphanumeric.lowercase
  _ <- Char.equal
  y <- String.fromArray <$> Repeat.until (S.anyChar) (C.lookAhead $ delimiters)
  pure $ Tuple x (marshall y)
  where
    delimiters = C.choice [Char.space *> pure unit, S.eof]

fields :: Parser String (Array (Tuple String Foreign))
fields = Array.fromFoldable <$> List.many field

body :: Parser String Foreign
body = marshall <$> fields
