module HW04.E1Test (tests) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Utils ((|>))

import qualified HW04.E1 as E1


tests :: TestTree
tests =
    testGroup "Homework 04"
        [ testGroup "Exercise 1"
            [ testProperty "fun1" $
                property $ do
                    list <- forAll genIntegrals
                    E1.fun1' list === E1.fun1 list
            , testCase "fun2" $
                E1.fun2' 27
                    |> assertEqual "" (E1.fun2 27)
            ]
        ]



-- Generators


genIntegral :: (MonadGen m, Integral a) => m a
genIntegral =
    Gen.integral (Range.linearFrom 0 (-100) 100)


genIntegrals :: (MonadGen m, Integral a) => m [a]
genIntegrals =
    Gen.list (Range.linear 0 10) genIntegral
