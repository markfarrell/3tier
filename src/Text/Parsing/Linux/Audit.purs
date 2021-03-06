module Text.Parsing.Linux.Audit
  ( entry
  ) where

import Prelude

import Data.Array as Array
import Data.List  as List

import Data.Foldable (intercalate)
import Data.Tuple (Tuple(..), fst, snd)

import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators as C
import Text.Parsing.Parser.String as S

import Foreign (Foreign)
import Foreign.Class (marshall)

import Text.Parsing.String.Repetition as R
import Text.Parsing.Combinators.Validation as V

import Text.Parsing.String.Alphanumeric as Alphanumeric
import Text.Parsing.Arguments.Value as Value

import Text.Parsing.Char as Char
import Text.String (fromChar)  as String

unquoted :: Parser String Unit -> Parser String String 
unquoted delimiters = do
  x <- R.until (V.not delimiters *> S.anyChar) delimiters
  _ <- delimiters
  pure x

quoted :: Parser String Unit -> Parser String Unit -> Parser String String 
quoted quote delimiters = do
  _ <- quote
  x <- R.until (V.not quote *> S.anyChar) quote
  _ <- quote
  _ <- delimiters
  pure x

argument :: Parser String String -> Parser String Unit -> Parser String String -> Parser String (Tuple String String)
argument name assignment value = do
  x <- name
  _ <- assignment
  y <- value
  pure $ Tuple x y

property :: Parser String (Tuple String String) -> Parser String (Tuple String Foreign)
property x = do
  y <- x
  pure $ Tuple (fst y) (marshall $ snd y)

msgField :: Parser String (Tuple String Foreign)
msgField = do
  x <- property $ argument name assignment (Value.spaced)
  _ <- delimiters
  pure x
  where
    name  = do
      x  <- Alphanumeric.lowercase
      xs <- Array.fromFoldable <$> List.many name'
      pure $ intercalate "" ([x] <> xs)
    name' :: Parser String String
    name' = do
      x <- String.fromChar <$> C.choice [Char.underscore, Char.hyphen] 
      y <- Alphanumeric.lowercase
      pure (x <> y)
    assignment = Char.equal *> pure unit
    delimiters = C.choice [S.char ' ' *> pure unit, S.eof]

messageMsg :: Parser String Foreign
messageMsg = marshall <$> Array.fromFoldable <$> List.many msgField

messageID :: Parser String Foreign
messageID = do
  x <- argument name assignment (unquoted delimiters)
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
  x <- property $ argument name assignment (unquoted delimiters)
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
