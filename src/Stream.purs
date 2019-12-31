module Stream
  ( Readable
  , Writable
  , pipe
  ) where

import Effect (Effect)

foreign import data Readable :: Type

foreign import data Writable :: Type

foreign import pipe :: Readable -> Writable -> Effect Writable
