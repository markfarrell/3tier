module FFI.UUIDv5
  ( namespaceUUID
  ) where

foreign import namespaceUUID :: String -> String -> String
