module Text.Parsing.String
  ( any
  ) where

import Prelude

import Data.Array as Array
import Data.List as List

import Text.Parsing.Parser (ParserT)
import Text.Parsing.Parser.String as S

import Text.String as String

any :: forall s m. S.StringLike s => Monad m => ParserT s m String
any = String.fromArray <$> Array.fromFoldable <$> List.many S.anyChar
