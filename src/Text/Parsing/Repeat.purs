module Text.Parsing.Repeat
  ( until
  , maximum
  , exact
  , minimum
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

{-- | Repeats a parser *u* at most *n* times. --}
maximum :: forall a b. Int -> Parser a b -> Parser a (Array b)
maximum = \n u -> case n > 0 of
  false  -> fail "Maximum repeat limit must be positive."
  true   -> maximum' [] n u
  where
    maximum' acc n u = case n > 0 of
      false -> pure acc
      true -> do
        x <- u 
        y <- maximum' (acc <> [x]) (n - 1) u
        pure y

{-- | Repeats a parser *u* exactly *n* times. --}
exact :: forall a b. Int -> Parser a b -> Parser a (Array b)
exact n = \u -> case n > 0 of
  false  -> fail "Exact repeat limit must be positive."
  true   -> exact' [] n u
  where
    exact' acc m u = case m > 0 of
      false -> case (n - m) == n of
        false -> fail $ "Invalid exact # of occurrences (" <> show m <> "," <> show n <> ")."
        true  -> pure acc
      true -> do
        x <- u 
        y <- exact' (acc <> [x]) (m - 1) u
        pure y

{-- | Repeats a parser *u* at least *n* times. --}
minimum :: forall a b. Int -> Parser a b -> Parser a (Array b)
minimum n = \u -> case n > 0 of
  false  -> fail "Minimum repeat limit must be positive."
  true   -> minimum' [] n u
  where
    minimum' acc m u = case m > 0 of
      false -> case (n - m) <= n of
        false -> fail $ "Invalid least # of occurrences (" <> show m <> "," <> show n <> ")."
        true  -> pure acc
      true -> do
        x <- u 
        y <- minimum' (acc <> [x]) (m - 1) u
        pure y
