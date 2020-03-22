module Test.UnitTest
  ( TestSuite
  , UnitTest
  , execute
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Traversable (sequence)

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)

import Effect.Exception as Exception

import Arrays as Arrays

type TestSuite = String

type UnitTest = String
  
execute' :: forall a b. TestSuite -> UnitTest -> Either a b -> Aff Unit
execute' testSuite unitTest (Left _)  = liftEffect $ do 
  result <- pure (Arrays.join " " ["FAILURE", testSuite, unitTest])
  _ <- log result 
  _ <- Exception.throw result
  pure unit
execute' testSuite unitTest (Right _) = liftEffect $ do 
  result <- pure (Arrays.join " " ["SUCCESS", testSuite, unitTest])
  _ <- log result
  pure unit

execute :: forall a b c. TestSuite -> UnitTest -> (a -> Aff (Either b c)) -> Array a -> Aff Unit
execute testSuite unitTest testFunction testCases = do 
  result <- sequence <$> (sequence (testFunction <$> testCases))
  _      <- execute' testSuite unitTest result
  pure unit
