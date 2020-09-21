module HW04.E2Test (tests) where

import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified HW04.E2 as EE


tests :: TestTree
tests =
    testGroup "Exercise 2: perfectly balanced binary tree"
        [ testProperty "balance factor should be less than or equal 1 (recursively)" $
            property $ do
                list <- forAll genIntegrals
                Hedgehog.assert $ (EE.balanced $ EE.foldTree list) == True
        , testProperty "height should be result as log 2 n" $
            property $ do
                list <- forAll genIntegrals
                let
                    heightOfBinaryBalancedTree = floor . logBase 2 . fromIntegral . length
                Hedgehog.assert $ (EE.height $ EE.foldTree list) == heightOfBinaryBalancedTree list
        ]



-- Generators


genIntegral :: (MonadGen m, Integral a) => m a
genIntegral =
    Gen.integral (Range.linear 0 100)


genIntegrals :: (MonadGen m, Integral a) => m [a]
genIntegrals =
    Gen.list (Range.linear 1 100) genIntegral
