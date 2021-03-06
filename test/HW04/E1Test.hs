module HW04.E1Test (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Utils ((|>))
import Test.Tasty.Hedgehog (testProperty)
import Hedgehog (MonadGen, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified HW04.E1 as EE


tests :: TestTree
tests =
    testGroup "Exercise 1"
        [ testProperty "fun1'" $
            property $ do
                list <- forAll genIntegrals
                EE.fun1' list === EE.fun1 list
        , testCase "fun2'" $
            EE.fun2' 27
                |> assertEqual "" (EE.fun2 27)
        ]



-- Generators


genIntegral :: (MonadGen m, Integral a) => m a
genIntegral =
    Gen.integral (Range.linearFrom 0 (-100) 100)


genIntegrals :: (MonadGen m, Integral a) => m [a]
genIntegrals =
    Gen.list (Range.linear 0 10) genIntegral
