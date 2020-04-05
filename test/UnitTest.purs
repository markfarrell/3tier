module Test.UnitTest
  ( UnitTest(..)
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

data UnitTest a b = UnitTest
  { eventCategory :: Audit.EventCategory
  , eventType     :: Audit.EventType
  , eventID       :: Audit.EventID
  , name          :: String
  , input         :: a
  , execute       :: (a -> Aff (Either Error b))
  }

instance showUnitTest :: Show (UnitTest a b) where
  show (UnitTest test) = intercalate " " [show test.eventType, show test.eventCategory, show test.eventID, test.name]

execute' :: forall a b. UnitTest a b -> Audit.EventType -> Aff Unit
execute' (UnitTest test) = \check -> liftEffect $ do
  _ <- log $ show (UnitTest test)
  case check == test.eventType of
    true   -> pure unit
    false  -> Exception.throw $ show (UnitTest test)

execute :: forall a b. UnitTest a b -> Aff (Either Error b)
execute (UnitTest test) = do 
  result <- test.execute $ test.input
  case result of
    (Left _)  -> execute' (UnitTest test) $ Audit.Failure 
    (Right _) -> execute' (UnitTest test) $ Audit.Success
  pure result
