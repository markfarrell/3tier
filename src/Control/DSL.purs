module Control.DSL
  ( Query(..)
  , Request
  , Result
  ) where

import Prelude

import Control.Monad.Free.Trans (FreeT)

import Data.Either (Either)

import Effect.Aff (Aff)
import Effect.Exception(Error)

data Query a b c d e = Forward a c (b -> e) | Report a d (b -> e)

type Request a b c d e = FreeT (Query a b c d) Aff e

type Result a = Either Error a

instance functorQuery :: Functor (Query a b c d) where
  map :: forall e e'. (e -> e') -> Query a b c d e -> Query a b c d e'
  map f (Forward settings query' next) = (Forward settings query' (f <<< next))
  map f (Report settings query' next)  = (Report settings query' (f <<< next))
