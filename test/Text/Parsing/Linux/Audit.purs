module Test.Text.Parsing.Linux.Audit
  ( suite
  ) where

import Prelude

import Effect.Aff (Aff)

import Text.Parsing.Linux.Audit as A

import Test.Text.Parsing as P

single :: Aff Unit
single = do
  _ <- P.assert true  "a=a"   A.single 
  _ <- P.assert true  "a=$"   A.single
  _ <- P.assert false "a="    A.single
  _ <- P.assert true  "a= "   A.single
  _ <- P.assert true  "a= :"  A.single
  _ <- P.assert false "a='"   A.single
  _ <- P.assert false "a=\""  A.single
  _ <- P.assert true  "a0=- " A.single
  pure unit

{-- https://github.com/markfarrell/3tier/issues/18 --}
suite :: Aff Unit
suite = do
  _ <- single
  pure unit
