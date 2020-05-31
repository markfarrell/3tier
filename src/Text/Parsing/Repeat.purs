module Text.Parsing.Repeat
  ( until
  , exact
  ) where

import Prelude

import Data.Maybe as M

import Text.Parsing.Parser (Parser, fail)

import Text.Parsing.Parser.Combinators as C

{-- | Repeats a parser *u*  until the result of a parser *v* is successful (consuming the result of *v*). --}
until :: forall a b c. Parser a b -> Parser a c -> Parser a (Array b)
until = until' []
  where
    until' acc u v = do
      x <- u 
      y <- C.optionMaybe v
      case M.isJust y of
        true  -> pure acc
        false -> until' (acc <> [x]) u v

{-- | Repeats a parser *u* exactly *n* times. --}
exact :: forall a b. Int -> Parser a b -> Parser a (Array b)
exact n = \u -> case n > 0 of
  false  -> fail "Exact repeat limit must be positive."
  true   -> exact' [] 0 u
  where
    exact' acc m u = case m == n of
      true  -> pure acc
      false -> do
        x <- u 
        y <- exact' (acc <> [x]) (m + 1) u
        pure y
