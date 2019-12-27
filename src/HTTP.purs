module HTTP
  ( IncomingRequest(..)
  , Server
  , IncomingMessage
  , ServerResponse
  , MessageHeaders
  , createServer
  , listen
  , messageMethod
  , messageURL
  , socket
  , messageHeaders
  , end
  , writeHead
  , setHeader
  , onRequest
  , write
  ) where

import Prelude

import Effect (Effect)

import Socket (Socket)

foreign import data Server :: Type
foreign import data IncomingMessage :: Type
foreign import data ServerResponse :: Type
foreign import data MessageHeaders :: Type

foreign import createServer :: Effect Server

foreign import listen :: Int -> Server -> Effect Unit

foreign import messageMethod :: IncomingMessage -> String

foreign import messageURL :: IncomingMessage -> String

foreign import socket :: IncomingMessage -> Socket

foreign import messageHeaders :: IncomingMessage -> MessageHeaders

foreign import showMessageHeadersImpl :: MessageHeaders -> String

foreign import end :: ServerResponse -> Effect Unit

foreign import writeHead :: Int -> ServerResponse -> Effect Unit

foreign import setHeader :: String -> String -> ServerResponse -> Effect Unit

foreign import onRequest :: (IncomingMessage -> ServerResponse -> Effect Unit) -> Server -> Effect Unit

foreign import write :: String -> ServerResponse -> Effect Unit

data IncomingRequest = IncomingRequest IncomingMessage ServerResponse

instance showMessageHeaders :: Show MessageHeaders where
  show = showMessageHeadersImpl
