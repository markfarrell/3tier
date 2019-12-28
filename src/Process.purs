module Process
  ( stdin
  , stdout
  ) where

import Stream as Stream

foreign import stdin :: Stream.Readable

foreign import stdout :: Stream.Writable
