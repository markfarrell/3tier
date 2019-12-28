module Process
  ( stdin
  , stdout
  , argv
  ) where

import Stream as Stream

foreign import stdin :: Stream.Readable

foreign import stdout :: Stream.Writable

foreign import argv :: Array String
