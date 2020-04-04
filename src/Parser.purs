module Parser
  ( digit
  , digits
  , positiveInteger
  , positiveFloat
  , anyString
  , date
  , lowercase
  , uppercase
  , octet
  , port
  , ipv4
  , json
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldMap, foldl)
import Data.List as List
import Data.String.CodeUnits (singleton)
import Data.Maybe (Maybe(..))

import Foreign (Foreign)

import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.String (string, anyChar)
import Text.Parsing.Parser.Combinators (choice)

import FFI.Date (Date)
import FFI.Date as Date
import FFI.JSON as JSON

import IPv4(IPv4(..))

foreign import parseInt :: String -> Int

foreign import parseFloat :: String -> Number

{-- Parses a valid digit, 0-9, or fails otherwise. --}
digit :: Parser String String
digit = choice (string <$> ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"])

digits :: Parser String String
digits = do
  result <- List.many digit
  pure $ foldMap identity result

positiveInteger :: Parser String Int
positiveInteger = do
  result <- parseInt <$> digits
  case result >= 0 of
    true  -> pure result
    false -> fail "Invalid positive integer."

positiveFloat :: Parser String Number
positiveFloat = do
  x      <- digits
  _      <- string "."
  y      <- digits
  z      <- pure (x <> "." <> y)
  result <- pure $ parseFloat z
  case result >= 0.0 of
    true  -> pure result
    false -> fail "Invalid positive float."

anyString :: Parser String String
anyString = foldMap singleton <$> List.many anyChar

date :: Parser String Date
date = do
  year   <- digits
  _      <- string "-"
  month  <- digits
  _      <- string "-"
  day    <- digits
  _      <- string "T"
  hour   <- digits
  _      <- string ":"
  minute <- digits
  _      <- string ":"
  second <- digits
  _      <- string "."
  millis <- digits
  _      <- string "Z"
  result <- pure $ format year month day hour minute second millis 
  case Date.parse result of
    (Left _)        -> fail "Invalid date."
    (Right result') -> pure $ result'
  where
    format year month day hour minute second millis = foldl (<>) year $
      [ "-"
      , month
      , "-"
      , day
      , "T"
      , hour
      , ":"
      , minute
      , ":"
      , second
      , "."
      , millis
      , "Z"
      ]

{-- | Parses a valid lowercase letter, or fails otherwise. --}
lowercase :: Parser String String
lowercase = choice (string <$> letters)
  where 
    letters = ["a", "b", "c", "d", "e", "f", "g", "h"]
      <> ["i", "j", "k", "l", "m", "n", "o", "p"] 
      <> ["q", "r", "s", "t", "u", "v", "w", "x"]
      <> ["y", "z"]

{-- | Parses a valid uppercase letter, or fails otherwise. --}
uppercase :: Parser String String
uppercase = choice (string <$> letters)
  where 
    letters = ["A", "B", "C", "D", "E", "F", "G", "H"]
      <> ["I", "J", "K", "L", "M", "N", "O", "P"] 
      <> ["Q", "R", "S", "T", "U", "V", "W", "Z"]
      <> ["Y", "Z"]

octet :: Parser String Int
octet = do
  w <- positiveInteger
  case Array.elemIndex w octets of
    (Just _)  -> pure w
    (Nothing) -> fail "Invalid octet."
  where octets = Array.range 0 255

port :: Parser String Int
port = do
  w <- positiveInteger
  case Array.elemIndex w ports of
    (Just _)  -> pure w
    (Nothing) -> fail "Invalid port."
  where ports = Array.range 0 65535

ipv4 :: Parser String IPv4
ipv4 = do
  w <- octet
  _ <- string dot
  x <- octet
  _ <- string dot
  y <- octet
  _ <- string dot
  z <- octet
  pure $ IPv4 w x y z
  where dot = "."

json :: Parser String Foreign
json = do
  x <- anyString
  y <- pure $ JSON.parse x
  case y of
    (Left _)  -> fail "Invalid JSON."
    (Right z) -> pure z 
