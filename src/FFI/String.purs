module FFI.String
  ( decodeURI
  , decodeURIComponent
  , encodeBase64
  , escape
  , encodeURIComponent
  , encodeURI
  ) where

foreign import decodeURI :: String -> String

foreign import decodeURIComponent :: String -> String

foreign import encodeBase64 :: String -> String

foreign import escapeImpl :: String -> String

foreign import encodeURIComponent :: String -> String

foreign import encodeURI :: String -> String

escape :: String -> String
escape = escapeImpl
