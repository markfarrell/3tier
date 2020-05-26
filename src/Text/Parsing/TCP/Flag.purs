module Text.Parsing.TCP.Flag
  ( flags ) where

import Prelude

import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.String (char)
import Text.Parsing.Parser.Combinators (choice, try)

import Data.TCP.Flag (Flag(..))

flags :: Parser String (Array Flag)
flags = do
  u <- urg
  r <- rst
  f <- fin
  s <- syn
  p <- psh
  a <- ack
  pure [u,r,f,s,p,a]
  where
    urg = choice [try (char 'U') *> pure (U true), pure (U false)]
    rst = choice [try (char 'R') *> pure (R true), pure (R false)]
    fin = choice [try (char 'F') *> pure (F true), pure (F false)]
    syn = choice [try (char 'S') *> pure (S true), pure (S false)]
    psh = choice [try (char 'P') *> pure (P true), pure (P false)]
    ack = choice [try (char 'A') *> pure (A true), pure (A false)]

