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

uri :: Forward -> String
uri (Flow entry)  = "/forward/flow/"  <> Flow.uri entry
uri (Audit entry) = "/forward/audit/" <> Audit.uri entry
