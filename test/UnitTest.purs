module Test.UnitTest
  ( UnitTest(..)
  , TestSuite
  , TestName
  , TestCase
  , TestFunction
  , TestInputs
  , execute
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Traversable (sequence)

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)

import Effect.Exception (Error)
import Effect.Exception as Exception

type TestSuite = String

type TestName = String

type TestCase = String

type TestFunction a b = a -> Aff (Either Error b)

type TestInputs a = Array a

type TestResult a = Either Error (Array a)

data UnitTest a b = UnitTest
  { testSuite    :: TestSuite
  , testName     :: TestName
  , testCase     :: TestCase
  , testFunction :: TestFunction a b
  , testInputs   :: TestInputs a
  }

execute' :: forall a b. UnitTest a b -> TestResult b -> Aff Unit
execute' (UnitTest unitTest) (Left _) = liftEffect $ do 
  result <- pure (intercalate " " ["FAILURE", unitTest.testSuite, unitTest.testName, unitTest.testCase])
  _ <- log result 
  _ <- Exception.throw result
  pure unit
execute' (UnitTest unitTest) (Right _) = liftEffect $ do 
  result <- pure (intercalate " " ["SUCCESS", unitTest.testSuite, unitTest.testName, unitTest.testCase])
  _ <- log result
  pure unit

execute :: forall a b. UnitTest a b -> Aff (TestResult b)
execute (UnitTest unitTest) = do 
  result <- sequence <$> (sequence (unitTest.testFunction <$> unitTest.testInputs))
  _      <- execute' (UnitTest unitTest) result
  pure result
