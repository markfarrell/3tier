module Test.Text.Parsing.Repeat
  ( suite
  ) where

import Prelude

import Effect.Aff (Aff)

import Text.Parsing.Parser.String as S

import Text.Parsing.Repeat as R

import Test.Text.Parsing as P

least :: Aff Unit
least = do
  _ <- P.failure "a"   (R.least  0 (S.char 'a'))
  _ <- P.success "a"   (R.least  1 (S.char 'a'))
  _ <- P.failure "a"   (R.least  2 (S.char 'a'))
  _ <- P.success "aa"  (R.least  2 (S.char 'a'))
  _ <- P.failure "a "  (R.least  2 (S.char 'a'))
  _ <- P.success "aaa"  (R.least  2 (S.char 'a'))
  pure unit

exact :: Aff Unit
exact = do
  _ <- P.failure "a"   (R.exact  0 (S.char 'a'))
  _ <- P.success "a"   (R.exact  1 (S.char 'a'))
  _ <- P.failure "a"   (R.exact  2 (S.char 'a'))
  _ <- P.success "aa"  (R.exact  2 (S.char 'a'))
  _ <- P.failure "a "  (R.exact  2 (S.char 'a'))
  _ <- P.failure "aaa" (R.exact  2 (S.char 'a'))
  pure unit

most :: Aff Unit
most = do
  _ <- P.failure "a"   (R.most  0 (S.char 'a'))
  _ <- P.success "a"   (R.most  1 (S.char 'a'))
  _ <- P.success "a"   (R.most  2 (S.char 'a'))
  _ <- P.success "aa"  (R.most  2 (S.char 'a'))
  _ <- P.success "a "  (R.most  2 (S.char 'a'))
  _ <- P.failure "aaa" (R.most  2 (S.char 'a'))
  _ <- P.success "aaa" (R.most  3 (S.char 'a'))
  pure unit

until :: Aff Unit
until = do
  _ <- P.failure  "a"  (R.until (S.char 'a') (S.char ' ')) 
  _ <- P.success  "a " (R.until (S.char 'a') (S.char ' ')) 
  _ <- P.success  "a " (R.until (S.satisfy (not <<< eq ' ')) (S.char ' ')) 
  _ <- P.success  "a " (R.until (S.char 'a') (S.char ' ') *> S.char (' ')) 
  pure unit

{-- https://github.com/markfarrell/3tier/issues/29 --}
suite :: Aff Unit
suite = do
  _ <- least
  _ <- exact
  _ <- most
  _ <- until
  pure unit
