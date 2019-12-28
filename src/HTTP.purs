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
  , Method(..)
  , IncomingResponse(..)
  , request
  ) where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

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

foreign import requestImpl :: (String -> ServerResponse -> IncomingResponse) -> String -> String -> EffectFnAff IncomingResponse

data Method = Get | Post

data IncomingRequest = IncomingRequest IncomingMessage ServerResponse

data IncomingResponse = IncomingResponse String ServerResponse

incomingResponse :: String -> ServerResponse -> IncomingResponse
incomingResponse body res = IncomingResponse body res

requestImpl' :: String -> String -> EffectFnAff IncomingResponse
requestImpl' = requestImpl incomingResponse

request :: Method -> String -> Aff IncomingResponse
request Get  = fromEffectFnAff <<< requestImpl' "GET"
request Post = fromEffectFnAff <<< requestImpl' "POST"

instance showMessageHeaders :: Show MessageHeaders where
  show = showMessageHeadersImpl
