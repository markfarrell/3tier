module RSA
  ( defaultEncrypt
  , defaultDecrypt
  , defaultSign
  , defaultVerify
  ) where

foreign import defaultEncrypt :: String -> String

foreign import defaultDecrypt :: String -> String

foreign import defaultSign :: String -> String

foreign import defaultVerify :: String -> String -> Boolean
