module Text.Parsing.Char
  ( space
  , colon
  , equal
  , comma
  , underscore
  , hyphen
  , singleQuote
  ) where

import Prelude

import Text.Parsing.Parser (ParserT)
import Text.Parsing.Parser.String as S

space :: forall s m. S.StringLike s => Monad m => ParserT s m Char
space = S.char ' '

colon :: forall s m. S.StringLike s => Monad m => ParserT s m Char
colon = S.char ':'

equal :: forall s m. S.StringLike s => Monad m => ParserT s m Char
equal = S.char '='

comma :: forall s m. S.StringLike s => Monad m => ParserT s m Char
comma = S.char ','

underscore :: forall s m. S.StringLike s => Monad m => ParserT s m Char
underscore = S.char '_'

hyphen :: forall s m. S.StringLike s => Monad m => ParserT s m Char
hyphen = S.char '-'

singleQuote :: forall s m. S.StringLike s => Monad m => ParserT s m Char
singleQuote = S.char '\''
