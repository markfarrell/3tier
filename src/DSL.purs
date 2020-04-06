module DSL
  ( DSL(..)
  , Request
  , Result
  ) where

import Prelude

import Control.Monad.Free.Trans (FreeT)
import Control.Monad.Writer.Trans(WriterT)

import Data.Either (Either)

import Effect.Aff (Aff)
import Effect.Exception(Error)

import Forward (Forward)
import Report (Report)

data DSL a b c = Forward a Forward (b -> c) | Report a Report (b -> c)

type Request a b c = FreeT (DSL a b) Aff c

type Result a = Either Error a

instance functorDSL :: Functor (DSL a b) where
  map :: forall c d. (c -> d) -> DSL a b c -> DSL a b d
  map f (Forward settings query' next) = (Forward settings query' (f <<< next))
  map f (Report settings query' next)  = (Report settings query' (f <<< next))
