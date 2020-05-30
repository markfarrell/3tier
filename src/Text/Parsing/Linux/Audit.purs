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

delimiters :: Parser String Unit
delimiters = C.choice [Char.space *> pure unit, Char.colon *> pure unit, S.eof]

single :: Parser String Audit.Entry
single = do
  x <- Alphanumeric.lowercase
  _ <- Char.equal
  _ <- Char.fail (C.lookAhead $ Char.quotes)
  y <- String.fromArray <$> Repeat.until (S.anyChar) (C.lookAhead $ delimiters)
  pure $ Tree.Single x y

{-- todo: partition :: Parser String Audit.Entry --}

{-- todo: index :: Parser String 
