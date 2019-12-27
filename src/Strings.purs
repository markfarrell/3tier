module Strings
  ( decodeURI
  , decodeURIComponent
  , encodeBase64
  , escape
  ) where

foreign import decodeURI :: String -> String

foreign import decodeURIComponent :: String -> String

foreign import encodeBase64 :: String -> String

foreign import escapeImpl :: String -> String

escape :: String -> String
escape = escapeImpl
