module Test.UnitTest
  ( UnitTest(..)
  , TestSuite
  , TestName
  , TestCase
  , TestFunction
  , TestResult(..)
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

type TestSuite = String

type TestName = String

type TestCase a = a

type TestFunction a b = a -> Aff (Either Error b)

data TestResult = Success | Failure

data UnitTest a b = UnitTest
  { suite   :: TestSuite
  , name    :: TestName
  , input   :: TestCase a
  , expect  :: TestResult 
  , execute :: TestFunction a b
  }

instance showTestResult :: Show TestResult where
  show Success = "SUCCESS"
  show Failure = "FAILURE"

instance eqTestResult :: Eq TestResult where
  eq Success Success = true
  eq Failure Failure = true
  eq _       _       = false

instance showUnitTest :: Show (UnitTest a b) where
  show (UnitTest test) = intercalate " " [show test.expect, test.suite, test.name]

execute' :: forall a b. UnitTest a b -> TestResult -> Aff Unit
execute' (UnitTest test) = \check -> liftEffect $ do
  _ <- log $ show (UnitTest test)
  case check == test.expect of
    true   -> pure unit
    false  -> Exception.throw $ show (UnitTest test)

execute :: forall a b. UnitTest a b -> Aff (Either Error b)
execute (UnitTest test) = do 
  result <- test.execute $ test.input
  case result of
    (Left _)  -> execute' (UnitTest test) $ Failure 
    (Right _) -> execute' (UnitTest test) $ Success
  pure result
