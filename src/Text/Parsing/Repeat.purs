module Text.Parsing.Repeat
  ( until
  , exact
  , least
  , most
  , range
  ) where

import Prelude


import Data.Array as A
import Data.List  as L

import Data.Maybe as M

import Text.Parsing.Parser (ParserT, fail)

import Text.Parsing.Parser.Combinators as C

-- | Consumes the current parse input with a parser `p` until the result of a parser `q` is successful.
-- | Does not consume the remaining parse input with the successful result of `q`.
until :: forall m a b c. Monad m => ParserT a m b -> ParserT a m c -> ParserT a m (Array b)
until = until' []
  where
    until' acc p q = do
      x <- p 
      y <- C.optionMaybe $ C.lookAhead q
      case M.isJust y of
        true  -> pure acc
        false -> until' (acc <> [x]) p q

-- | Consumes the current parse input with a parser `p`, with `n > 0` repetitions of `p`.
-- | Does not check if the remaining parse input can be moreover parsed with `p`.
least :: forall a m b. Monad m => Int -> ParserT a m b -> ParserT a m (Array b)
least n = \p -> case n > 0 of
  false  -> fail "Least number of repetitions must be positive."
  true   -> least' [] 0 p
  where
    least' acc m p = case m == n of
      true  -> pure acc
      false -> do
        x <- p 
        y <- least' (acc <> [x]) (m + 1) p
        pure y

-- | Consumes the current parse input with a parser `p`, with `n > 0` repetitions of `p`.
-- | Fails if the remaining parse input can be moreover be parsed with `p`.
exact :: forall a m b. Monad m => Int -> ParserT a m b -> ParserT a m (Array b)
exact n = \p -> do
  x <- least n $ p
  y <- C.optionMaybe p
  case M.isJust y of
    true  -> fail $ "Number of repetitions must be " <> show n <> "."
    false -> pure x 

-- | Consumes the current parse input with a parser `p`, with `m` greedy repetitions of `p`.
-- | Fails if `m` is greater than the constraint `n > 0` passed to the function. 
most :: forall a m b. Monad m => Int -> ParserT a m b -> ParserT a m (Array b)
most n p = case n > 0 of
 false ->  fail $ "Most number of repetitions must be positive."
 true -> do
   x <- A.fromFoldable <$> L.many p
   m <- pure $ A.length x
   case m > n of
     true  -> fail $ "Number of repetitions must be at most " <> show n <> "." 
     false -> pure x

-- | Consumes the current parse input with a parser `p`, with at *least* `min` and at *most* `max` repetitions of `p`.
range :: forall a m b. Monad m => Int -> Int -> ParserT a m b -> ParserT a m (Array b)
range min max = do
  x <- least min
  y <- most (max - min)
  pure (x <> y)
