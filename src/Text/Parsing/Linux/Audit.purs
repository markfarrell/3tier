module Text.Parsing.Linux.Audit
  ( entry
  ) where

import Prelude

import Data.Array as Array
import Data.List as List

import Data.Foldable (intercalate)
import Data.Tuple (Tuple(..), fst, snd)

import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators as C
import Text.Parsing.Parser.String as S

import Foreign (Foreign)
import Foreign.Class (marshall)

import Text.Parsing.Alphanumeric as Alphanumeric
import Text.Parsing.Char as Char
import Text.Parsing.Repeat as Repeat

import Text.String as String

argument :: Parser String String -> Parser String Unit -> Parser String Unit -> Parser String (Tuple String String)
argument name assignment delimiters = do
  x <- name
  _ <- assignment
  y <- String.fromArray <$> Repeat.until (S.anyChar) delimiters
  pure $ Tuple x y

property :: Parser String String -> Parser String Unit -> Parser String Unit -> Parser String (Tuple String Foreign)
property name assignment delimiters = do
  x <- argument name assignment delimiters
  pure $ Tuple (fst x) (marshall $ snd x)

msgField :: Parser String (Tuple String Foreign)
msgField = property name assignment delimiters
  where
    name  = do
      x  <- Alphanumeric.lowercase
      xs <- Array.fromFoldable <$> List.many name'
      pure $ intercalate underscore ([x] <> xs)
    name' = do
      _ <- Char.underscore
      x <- Alphanumeric.lowercase
      pure x
    assignment = Char.equal *> pure unit
    delimiters = C.choice [Char.space *> pure unit, S.eof]
    underscore = "_"

messageMsg :: Parser String Foreign
messageMsg = marshall <$> Array.fromFoldable <$> List.many msgField

messageID :: Parser String Foreign
messageID = do
  x <- argument name assignment delimiters
  _ <- Char.colon
  pure $ id x
  where
    id         = \x -> marshall $ intercalate "" ["(", "audit", snd x, ")"]
    name       = S.string "msg"
    assignment = do
      _ <- Char.equal
      _ <- S.string "audit"
      _ <- S.char '('
      pure unit                  
    delimiters = S.char ')' *> pure unit

messageType :: Parser String Foreign
messageType = do
  x <- property name assignment delimiters
  pure $ snd x
  where
    name       = S.string "type"
    assignment = Char.equal *> pure unit
    delimiters = C.choice [Char.space *> pure unit, S.eof]

entry :: Parser String Foreign
entry = do
  w <- messageType
  x <- messageID 
  _ <- Char.space
  y <- messageMsg
  _ <- S.eof
  z <- pure $
   [ Tuple "type" w
   , Tuple "id"   x
   , Tuple "msg"  y 
   ]
  pure $ marshall z
