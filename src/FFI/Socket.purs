module FFI.Socket
  ( Socket
  , remoteAddress
  , remotePort
  ) where

foreign import data Socket :: Type

foreign import remoteAddress :: Socket -> String

foreign import remotePort :: Socket -> Int
