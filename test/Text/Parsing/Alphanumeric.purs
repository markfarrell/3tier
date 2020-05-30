module Test.Text.Parsing.Alphanumeric
  ( suite
  ) where

import Prelude

import Effect.Aff (Aff)

import Text.Parsing.Alphanumeric as A

import Test.Text.Parsing as P

suite :: Aff Unit
suite = do
  _ <- P.assert true  "abcdefghijklmnopqrstuvwxyz0123456789" A.lowercase
  _ <- P.assert false "ABCDEFGHIJKLMNOPQRSTUVWXYZ" A.lowercase
  _ <- P.assert false "$..." A.lowercase
  _ <- P.assert true  "aA" A.lowercase
  _ <- P.assert true  "type=" A.lowercase
  _ <- P.assert true  "type=DAEMON_START" A.lowercase
  _ <- P.assert true  "type=DAEMON_START msg=audit(1589480737.598:9117)" A.lowercase
  pure unit
