module Text.Parsing.Linux.Audit
  ( entry
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

arg :: Parser String String -> Parser String Unit -> Parser String Unit -> Parser String (Tuple String Foreign)
arg name assignment delimiters = do
  x <- name
  _ <- assignment
  y <- String.fromArray <$> Repeat.until (S.anyChar) delimiters
  pure $ Tuple x (marshall y)

field :: Parser String (Tuple String Foreign)
field = arg name assignment delimiters
  where
    name       = Alphanumeric.lowercase
    assignment = Char.equal *> pure unit
    delimiters = C.choice [Char.space *> pure unit, S.eof]

fields :: Parser String (Array (Tuple String Foreign))
fields = Array.fromFoldable <$> List.many field

msg :: Parser String (Tuple String Foreign)
msg = do
  x <- arg name assignment delimiters
  _ <- Char.colon
  pure x
  where
    name       = S.string "msg"
    assignment = do
      _ <- Char.equal
      _ <- S.string "audit"
      _ <- S.char '('
      pure unit                  
    delimiters = S.char ')' *> pure unit

messageType :: Parser String (Tuple String Foreign)
messageType = arg name assignment delimiters
  where
    name       = S.string "type"
    assignment = Char.equal *> pure unit
    delimiters = C.choice [Char.space *> pure unit, S.eof]

entry :: Parser String (Tuple Foreign Foreign)
entry = do
  x <- messageType
  y <- msg 
  _ <- Char.space
  z <- fields
  _ <- S.eof
  pure $ Tuple (marshall [x,y]) (marshall z)
