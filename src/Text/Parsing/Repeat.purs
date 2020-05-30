module Text.Parsing.Repeat
  ( until
  ) where

import Prelude

import Data.Maybe as M

import Text.Parsing.Parser (Parser, fail)

import Text.Parsing.Parser.Combinators as C

{-- | Repeats a parser *u* at most *n* times until the result of a parser *v* is successful (consuming the result of *v*). --}
until :: forall a b c. Int -> Parser a b -> Parser a c -> Parser a (Array b)
until = \n u v -> case n > 0 of
  true  -> until' [] n u v
  false -> fail "Limit must be positive." 
  where
    until' acc n u v = case n > 0 of
      false -> pure acc
      true  -> do
        x <- u 
        y <- C.optionMaybe v
        case M.isJust y of
          true  -> pure acc
          false -> until' (acc <> [x]) (n - 1) u v
