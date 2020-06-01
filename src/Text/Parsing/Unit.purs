module Text.Parsing.Unit
  ( fail
  ) where

import Prelude

import Data.Maybe as Maybe

import Text.Parsing.Parser (ParserT)
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as C
import Text.Parsing.Parser.String as S

fail :: forall s m a. S.StringLike s => Monad m => ParserT s m a -> ParserT s m Unit
fail x = do
  y <- C.optionMaybe x 
  case Maybe.isJust y of
    true  -> P.fail $ "Parsed an invalid result."
    false -> pure unit
