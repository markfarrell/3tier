module Test.Test
  ( Test(..)
  , execute
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (intercalate)

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)

import Effect.Exception (Error)
import Effect.Exception as Exception

import Audit as Audit

data Test a = Test
  { eventCategory :: Audit.EventCategory
  , eventType     :: Audit.EventType
  , eventID       :: Audit.EventID
  , eventURI      :: Audit.EventURI
  , execute       :: Unit -> Aff (Either Error a)
  }

instance showTest :: Show (Test a) where
  show (Test test) = intercalate " " [show test.eventType, show test.eventCategory, show test.eventID, test.eventURI]

execute :: forall a. Test a -> Aff (Either Error a)
execute = \(Test test) -> do 
  result <- test.execute unit
  case result of
    (Left _)  -> audit Audit.Failure $ Test test 
    (Right _) -> audit Audit.Success $ Test test
  pure result
  where
    audit check = \(Test test) ->liftEffect $ do
      _ <- log $ show (Test test)
      case check == test.eventType of
        true   -> pure unit
        false  -> Exception.throw $ show (Test test)
