module Text.Parsing.Linux.Audit
  ( single
  ) where

import Prelude

import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators as C
import Text.Parsing.Parser.String as S

import Data.Linux.Audit.Entry as Audit
import Data.Tree as Tree

import Text.Parsing.Alphanumeric as Alphanumeric
import Text.Parsing.Char as Char
import Text.Parsing.Repeat as Repeat

import Text.String as String

single :: Parser String Unit -> Parser String Audit.Entry
single delimiter = do
  x <- Alphanumeric.lowercase
  _ <- Char.equal
  y <- C.choice [String.fromArray <$> Repeat.until (S.anyChar) (C.lookAhead delimiter)]
  pure $ Tree.Single x y
