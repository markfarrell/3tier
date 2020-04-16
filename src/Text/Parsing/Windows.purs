module Text.Parsing.Windows
  ( event 
  ) where

import Prelude

import Control.Monad.Except (runExcept)

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Effect.Exception (error, Error) as Exception

import Foreign (Foreign)
import Foreign as Foreign
import Foreign.Index ((!))

import Text.Parsing.Parser (Parser, fail, runParser)
import Text.Parsing.Parser.Combinators (choice)
import Text.Parsing.Parser.String (string)

import Data.Windows as Windows
import Text.Parsing.Common (date, json, ipv4, port, positiveInteger)

event :: Parser String Windows.Event
event = do
  x <- json
  y <- pure $ read x
  case y of
    (Left _)  -> fail "Invalid input."
    (Right z) -> pure z

read :: Foreign -> Either Exception.Error Windows.Event
read = \x -> do
  eventCategory' <- read' "eventCategory" eventCategory $ x
  eventID'       <- read' "eventID" eventID $ x
  eventType'     <- read' "eventType" eventType $ x
  startTime'     <- read' "startTime" date $ x
  duration'      <- read' "duration"  positiveInteger $ x
  endTime'       <- read' "endTime" date $ x
  sIP'           <- read' "sIP" ipv4 $ x
  sPort'         <- read' "sPort" port $ x
  case eventID' of
    (Tuple eventCategory'' eventID'') -> do
      case eventCategory' == eventCategory'' of
        true  -> pure $ Windows.Event
          { eventCategory : eventCategory''
          , eventID       : eventID''
          , eventType     : eventType'
          , startTime     : startTime'
          , duration      : duration'
          , endTime       : endTime'
          , sIP           : sIP'
          , sPort         : sPort'
          }
        false -> Left (Exception.error "Invalid input.")

read' :: forall a. String -> Parser String a -> Foreign -> Either Exception.Error a
read' = \x y z -> do
  result  <- runExcept' (z ! x >>= Foreign.readString)
  result' <- run result y
  pure result'
  where 
    runExcept' = \x -> do
      result <- pure $ runExcept x
      case result of
        (Left _)    -> Left (Exception.error "Invalid raw input.")
        (Right raw) -> pure raw 
    run = \x y -> do
      result <- pure $ runParser x y
      case result of
        (Left _)    -> Left (Exception.error "Invalid input.")
        (Right val) -> pure val

eventID :: Parser String (Tuple Windows.EventCategory Windows.EventID)
eventID = choice (eventID' <$> Windows.eventCategories)
  where
    eventID' = \eventCategory' -> do
      result <- positiveInteger
      case Array.elemIndex result (Windows.eventIDs eventCategory') of
        (Just _)  -> pure (Tuple eventCategory' result)
        (Nothing) -> fail "Invalid input."

eventCategory :: Parser String Windows.EventCategory
eventCategory = choice (eventCategory' <$> Windows.eventCategories)
  where
    eventCategory' = \x -> do
       _ <- string (show x) 
       pure x

eventType :: Parser String Windows.EventType
eventType = choice (eventType' <$> Windows.eventTypes)
  where
    eventType' = \x -> do
       _ <- string (show x) 
       pure x
