module Parser
  ( digit
  , digits
  , positiveInteger
  , positiveFloat
  , anyString
  , timestamp
  , lowercase
  , uppercase
  ) where

import Prelude

import Data.Foldable (foldMap, foldl)
import Data.List as List
import Data.String.CodeUnits (singleton)

import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.String (string, anyChar)
import Text.Parsing.Parser.Combinators (choice, optional)

import Date as Date

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

timestamp :: Parser String String
timestamp = do
  year   <- digits
  _      <- choice [string "/", string "-"]
  month  <- digits
  _      <- choice [string "/", string "-"]
  day    <- digits
  _      <- string "T"
  hour   <- digits
  _      <- string ":"
  minute <- digits
  _      <- string ":"
  second <- digits
  _      <- string "."
  millis <- digits
  _      <- optional $ string "Z"
  result <- pure $ format year month day hour minute second millis 
  case Date.isValid result of
    true  -> pure $ result
    false -> fail "Invalid sTime or eTime."
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

