module Text.Parsing.Char
  ( quote
  , space
  , colon
  , equal
  , comma
  ) where

import Prelude

import Text.Parsing.Parser (ParserT)
import Text.Parsing.Parser.String as S

import Data.Quote (Quote(..))

quote :: forall s m. S.StringLike s => Monad m => Quote -> ParserT s m Char
quote Single = S.char '\''
quote Double = S.char '"'

space :: forall s m. S.StringLike s => Monad m => ParserT s m Char
space = S.char ' '

colon :: forall s m. S.StringLike s => Monad m => ParserT s m Char
colon = S.char ':'

equal :: forall s m. S.StringLike s => Monad m => ParserT s m Char
equal = S.char '='

comma :: forall s m. S.StringLike s => Monad m => ParserT s m Char
comma = S.char ','
