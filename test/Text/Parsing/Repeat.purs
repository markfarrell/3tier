module Test.Text.Parsing.Repeat
  ( suite
  ) where

import Prelude

import Effect.Aff (Aff)

import Text.Parsing.Parser.String as S

import Text.Parsing.Repeat as R

import Test.Text.Parsing as P

suite :: Aff Unit
suite = do
  _ <- P.assert false "a "  (R.until 0 (S.char 'a') (S.char ' ')) 
  _ <- P.assert true  "a "  (R.until 1 (S.char 'a') (S.char ' ')) 
  _ <- P.assert false "a "  (R.until 1 (S.char 'a') (S.char ' ') *> S.char (' ')) 
  _ <- P.assert true  "a  " (R.until 1 (S.char 'a') (S.char ' ') *> S.char (' ')) 
  _ <- P.assert true  "a  " (R.until 2 (S.anyChar)  (S.char ' ') *> S.char (' '))
  pure unit
