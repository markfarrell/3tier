module FFI.Readline
  ( Interface
  , createInterface
  , onLine
  ) where

import Prelude

import Effect (Effect)

import FFI.Stream as Stream

foreign import data Interface :: Type

foreign import createInterface :: Stream.Readable -> Stream.Writable -> Boolean -> Effect Interface

foreign import onLine :: (String -> Effect Unit) -> Interface -> Effect Unit
