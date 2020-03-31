module Forward
  (Forward(..)
  , uri
  ) where

import Prelude

import Audit as Audit
import Flow as Flow

import Data.Either (Either(..))

import Effect.Exception (Error)
import Effect.Exception as Exception

import Strings as Strings

data Forward = Flow Flow.Entry | Audit Audit.Entry

uri :: Forward -> Either Error String
uri (Flow entry) = do
  query <- Flow.write entry
  pure ("/forward/flow/" <> Strings.encodeURIComponent query)
uri (Audit entry) = Left $ Exception.error "Unexpected behaviour."
