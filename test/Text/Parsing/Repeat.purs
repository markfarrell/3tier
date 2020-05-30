module Test.Text.Parsing.Repeat
  ( suite
  ) where

import Prelude

import Effect.Aff (Aff)

import Text.Parsing.Parser.String as S

import Text.Parsing.Repeat as R

import Test.Text.Parsing as P

{-- https://github.com/markfarrell/3tier/issues/29 --}
suite :: Aff Unit
suite = do
  _ <- P.assert true  "a "  (R.until (S.char 'a') (S.char ' ')) 
  _ <- P.assert true  "a "  (R.until (S.satisfy (not <<< eq ' ')) (S.char ' ')) 
  _ <- P.assert false "a "  (R.until (S.char 'a') (S.char ' ') *> S.char (' ')) 
  _ <- P.assert true  "a  " (R.until (S.char 'a') (S.char ' ') *> S.char (' ')) 
  _ <- P.assert true  "a  " (R.until (S.anyChar)  (S.char ' ') *> S.char (' '))
  _ <- P.assert false "a"   (R.maximum 0 (S.char 'a'))
  _ <- P.assert true  "a"   (R.maximum 1 (S.char 'a'))
  _ <- P.assert true  "aa"  (R.maximum 1 (S.char 'a'))
  _ <- P.assert false "a"   (R.maximum 2 (S.char 'a'))
  _ <- P.assert false "a"   (R.exact   0 (S.char 'a'))
  _ <- P.assert true  "a"   (R.exact   1 (S.char 'a'))
  _ <- P.assert false "a"   (R.exact   2 (S.char 'a'))
  _ <- P.assert true  "aa"  (R.exact   2 (S.char 'a'))
  _ <- P.assert false "a "  (R.exact   2 (S.char 'a'))
  _ <- P.assert false "a"   (R.minimum 0 (S.char 'a')) 
  _ <- P.assert true  "a"   (R.minimum 1 (S.char 'a')) 
  _ <- P.assert false "a"   (R.minimum 2 (S.char 'a')) 
  pure unit
