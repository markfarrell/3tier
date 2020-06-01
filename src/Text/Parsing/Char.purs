module Text.Parsing.Char
  ( quote
  , quotes
  , space
  , colon
  , equal
  , comma
  , underscore
  ) where

import Prelude

import Data.Array as Array

import Data.Maybe as Maybe

import Text.Parsing.Parser (ParserT)
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as C
import Text.Parsing.Parser.String as S

import Data.Quote (Quote(..))

underscore :: forall s m. S.StringLike s => Monad m => ParserT s m Char
underscore = S.char '_'

quote :: forall s m. S.StringLike s => Monad m => Quote -> ParserT s m Char
quote Single = S.char '\''
quote Double = S.char '"'

quotes :: forall s m. S.StringLike s => Monad m => ParserT s m Char
quotes = C.choice [quote Single, quote Double]

space :: forall s m. S.StringLike s => Monad m => ParserT s m Char
space = S.char ' '

colon :: forall s m. S.StringLike s => Monad m => ParserT s m Char
colon = S.char ':'

equal :: forall s m. S.StringLike s => Monad m => ParserT s m Char
equal = S.char '='

comma :: forall s m. S.StringLike s => Monad m => ParserT s m Char
comma = S.char ','
