module UDP
  ( Socket
  , createSocket
  , bind
  , onMessage
  , send
  ) where

import Prelude
import Effect (Effect)

foreign import data Socket :: Type

foreign import createSocket :: Effect Socket

foreign import bind :: Socket -> Int -> Effect Unit

foreign import onMessage :: (String -> Effect Unit) -> Socket -> Effect Unit

foreign import send :: Socket -> String -> Int -> String -> Effect Unit
