module Text.Parsing.Common
  ( digit
  , digits
  , nonnegativeInteger
  , nonnegativeFloat
  , anyString
  , date
  , lowercase
  , uppercase
  , octet
  , port
  , ipv4
  , flags
  , json
  , property
  , propertyNot
  , showable
  , substring
  , readArray
  ) where

import Prelude

import Control.Monad.Except (runExcept)

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldMap, foldl)
import Data.List as List
import Data.String.CodeUnits (singleton)
import Data.Maybe (Maybe(..))

import Foreign (Foreign)
import Foreign as Foreign
import Foreign.Index ((!))

import Text.Parsing.Parser (Parser, fail, runParser)
import Text.Parsing.Parser.String (string, anyChar, char)
import Text.Parsing.Parser.Combinators (optional,choice,try,lookAhead)

import FFI.Date (Date)
import FFI.Date as Date
import FFI.JSON as JSON

import Data.IPv4(IPv4(..))
import Data.TCP.Flag (Flag(..))

foreign import parseInt      :: String -> Int

foreign import parseFloat    :: String -> Number

foreign import indexOf       :: String -> String -> Int

foreign import substringImpl :: String -> Int -> Int -> String

{-- Parses a valid digit, 0-9, or fails otherwise. --}
digit :: Parser String String
digit = choice (string <$> ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"])

digits :: Parser String String
digits = do
  result <- List.many digit
  pure $ foldMap identity result

nonnegativeInteger :: Parser String Int
nonnegativeInteger = do
  result <- parseInt <$> digits
  case result >= 0 of
    true  -> pure result
    false -> fail "Invalid nonnegative integer."

nonnegativeFloat :: Parser String Number
nonnegativeFloat = do
  x      <- digits
  _      <- string "."
  y      <- digits
  z      <- pure (x <> "." <> y)
  result <- pure $ parseFloat z
  case result >= 0.0 of
    true  -> pure result
    false -> fail "Invalid nonnegative float."

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
  w <- nonnegativeInteger
  case Array.elemIndex w octets of
    (Just _)  -> pure w
    (Nothing) -> fail "Invalid octet."
  where octets = Array.range 0 255

port :: Parser String Int
port = do
  w <- nonnegativeInteger
  case Array.elemIndex w ports of
    (Just _)  -> pure w
    (Nothing) -> fail "Invalid port."
  where ports = Array.range 0 65535

ipv4 :: Parser String IPv4
ipv4 = do
  _ <- optional $ choice [string "::ffff:", string "::"]
  w <- octet
  _ <- string dot
  x <- octet
  _ <- string dot
  y <- octet
  _ <- string dot
  z <- octet
  pure $ IPv4 w x y z
  where dot = "."

flags :: Parser String (Array Flag)
flags = do
  u <- urg
  r <- rst
  f <- fin
  s <- syn
  p <- psh
  a <- ack
  pure [u,r,f,s,p,a]
  where
    urg = choice [try (char 'U') *> pure (U true), pure (U false)]
    rst = choice [try (char 'R') *> pure (R true), pure (R false)]
    fin = choice [try (char 'F') *> pure (F true), pure (F false)]
    syn = choice [try (char 'S') *> pure (S true), pure (S false)]
    psh = choice [try (char 'P') *> pure (P true), pure (P false)]
    ack = choice [try (char 'A') *> pure (A true), pure (A false)]

json :: Parser String Foreign
json = do
  x <- anyString
  y <- pure $ JSON.parse x
  case y of
    (Left _)  -> fail "Invalid JSON."
    (Right z) -> pure z 

readString :: String -> Foreign -> Parser String String
readString name obj = do
  input <- except (obj ! name >>= Foreign.readString)
  pure input
  where 
    except = \x -> do
      result <- pure $ runExcept x
      case result of
        (Left _)    -> fail $ "Property not found: \"" <> name <> "\" (readString)"
        (Right raw) -> pure raw 

readInt :: String -> Foreign -> Parser String String
readInt name obj = do
  input <- except (obj ! name >>= Foreign.readInt)
  pure $ show input
  where 
    except = \x -> do
      result <- pure $ runExcept x
      case result of
        (Left _)    -> fail $ "Property not found: \"" <> name <> "\" (readInt)"
        (Right raw) -> pure raw 

readArray :: String -> Foreign -> Parser String (Array Foreign)
readArray name obj = do
  input <- except (obj ! name >>= Foreign.readArray)
  pure $ input
  where 
    except = \x -> do
      result <- pure $ runExcept x
      case result of
        (Left _)    -> fail $ "Property not found: \"" <> name <> "\" (readArray)"
        (Right raw) -> pure raw 

property :: forall a. String -> Foreign -> Parser String a -> Parser String a
property = \x y z -> do
  input   <- choice [readString x y, readInt x y]
  result' <- validation x input z
  pure result'
  where 
    validation = \x y z -> do
      result <- pure $ runParser y z
      case result of
        (Left e)    -> fail $ "Invalid foreign property (" <> x <> ")."
        (Right val) -> pure val

propertyNot :: forall a. String -> Foreign -> Parser String a -> Parser String String
propertyNot = \x y z -> do
  input   <- choice [readString x y]
  result' <- validation x input z
  pure result'
  where 
    validation = \x y z -> do
      result <- pure $ runParser y z
      case result of
        (Right _)   -> fail $ "Invalid foreign property (" <> x <> ")."
        (Left _)    -> pure y

showable :: forall a. Show a => Array a -> Parser String a
showable = \x -> choice (show' <$> x)
  where
    show' = \x -> do
      _ <- string (show x)
      pure x

substring :: String -> Parser String String
substring = \x -> do
  y <- lookAhead $ anyString
  z <- pure $ indexOf y x 
  case z == -1 of
    true  -> fail $ "Substring not found (" <> x <> "," <> y <> ")"
    false -> string (substringImpl y 0 z) *> string x
