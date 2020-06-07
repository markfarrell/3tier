module Text.Parsing.Common
  ( digit
  , digits
  , nonnegativeInteger
  , nonnegativeFloat
  , date
  , octet
  , port
  , ipv4
  , json
  , property
  , validation
  , array
  , substring
  , readArray
  , readIndex
  , readString
  , uuid
  ) where

import Prelude

import Control.Monad.Except (runExcept)

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldMap, foldl)
import Data.List as List
import Data.Maybe (Maybe(..))

import Foreign (Foreign)
import Foreign as Foreign
import Foreign.Index ((!))

import Text.Parsing.Parser (Parser, fail, runParser)
import Text.Parsing.Parser.String (string)
import Text.Parsing.Parser.Combinators (optional,choice,lookAhead)

import Unsafe.Coerce (unsafeCoerce)

import FFI.Date (Date)
import FFI.Date as Date
import FFI.JSON as JSON
import FFI.UUID (UUID)

import Data.IPv4 (IPv4(..))
import Data.Port (Port) 

import Text.Parsing.String as String

import Text.Parsing.String.UUID as UUID


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

octet :: Parser String Int
octet = do
  w <- nonnegativeInteger
  case Array.elemIndex w octets of
    (Just _)  -> pure w
    (Nothing) -> fail "Invalid octet."
  where octets = Array.range 0 255

port :: Parser String Port
port = do
  w <- nonnegativeInteger
  case Array.elemIndex w ports of
    (Just _)  -> pure $ unsafeCoerce w
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

json :: Parser String Foreign
json = do
  x <- String.any
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

readInt :: String -> Foreign -> Parser String Int
readInt name obj = do
  input <- except (obj ! name >>= Foreign.readInt)
  pure input
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

readIndex :: String -> Foreign -> Parser String Foreign
readIndex name obj = do
  input <- except (obj ! name)
  pure $ input
  where 
    except = \x -> do
      result <- pure $ runExcept x
      case result of
        (Left _)    -> fail $ "Property not found: \"" <> name <> "\" (readIndex)"
        (Right raw) -> pure raw 

property :: forall a. String -> Foreign -> Parser String a -> Parser String a
property = \x y z -> do
  input   <- choice [readString x y]
  result' <- parse x input z
  pure result'
  where 
    parse = \x y z -> do
      result <- pure $ runParser y z
      case result of
        (Left e)    -> fail $ "Invalid foreign property (" <> x <> ")."
        (Right val) -> pure val

validation :: forall a. String -> Foreign -> Parser String a -> Parser String String
validation = \x y z -> do
  input   <- choice [readString x y]
  result' <- validation' x input z
  pure result'
  where 
    validation' = \x y z -> do
      result <- pure $ runParser y z
      case result of
        (Right _)   -> fail $ "Invalid foreign property (" <> x <> ")."
        (Left _)    -> pure y

array :: forall a. Show a => Array a -> Parser String a
array = \x -> choice (show' <$> x)
  where
    show' = \x -> do
      _ <- string (show x)
      pure x

substring :: String -> Parser String String
substring = \x -> do
  y <- lookAhead $ String.any
  z <- pure $ indexOf y x 
  case z == -1 of
    true  -> fail $ "Substring not found (" <> x <> "," <> y <> ")"
    false -> string (substringImpl y 0 z) *> string x
    
uuid :: Parser String UUID
uuid = unsafeCoerce <$> UUID.any
