module RSA
  ( defaultEncrypt
  , defaultDecrypt
  ) where

foreign import defaultEncrypt :: String -> String

foreign import defaultDecrypt :: String -> String
