module HTTP
  ( IncomingRequest(..)
  , Server
  , IncomingMessage
  , ServerResponse
  , MessageHeaders
  , ClientRequest
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
  , createRequest
  , setRequestHeader
  , endRequest
  ) where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)

import Foreign (Foreign)

import Socket (Socket)

foreign import data Server :: Type
foreign import data IncomingMessage :: Type
foreign import data ServerResponse :: Type
foreign import data ClientRequest :: Type

type MessageHeaders = Foreign

foreign import createServer :: Effect Server

foreign import listen :: Int -> Server -> Effect Unit

foreign import messageMethod :: IncomingMessage -> String

foreign import messageURL :: IncomingMessage -> String

foreign import socket :: IncomingMessage -> Socket

foreign import messageHeaders :: IncomingMessage -> MessageHeaders

foreign import end :: ServerResponse -> Effect Unit

foreign import writeHead :: Int -> ServerResponse -> Effect Unit

foreign import setHeader :: String -> String -> ServerResponse -> Effect Unit

foreign import onRequest :: (IncomingMessage -> ServerResponse -> Effect Unit) -> Server -> Effect Unit

foreign import write :: String -> ServerResponse -> Effect Unit

foreign import createRequestImpl :: String -> String -> Effect ClientRequest

foreign import setRequestHeaderImpl :: String -> String -> ClientRequest -> Effect Unit

foreign import endRequestImpl :: (String -> IncomingMessage -> IncomingResponse) -> ClientRequest -> EffectFnAff IncomingResponse

data Method = Get | Post

data IncomingRequest = IncomingRequest IncomingMessage ServerResponse

data IncomingResponse = IncomingResponse String IncomingMessage

incomingResponse :: String -> IncomingMessage -> IncomingResponse
incomingResponse body res = IncomingResponse body res

createRequest :: Method -> String -> Aff ClientRequest
createRequest Get  = liftEffect <<< createRequestImpl "GET"
createRequest Post = liftEffect <<< createRequestImpl "POST"

setRequestHeader :: String -> String -> ClientRequest -> Aff Unit
setRequestHeader headerName headerValue req = liftEffect $ setRequestHeaderImpl headerName headerValue req

endRequest :: ClientRequest -> Aff IncomingResponse
endRequest client = fromEffectFnAff $ endRequestImpl incomingResponse client
