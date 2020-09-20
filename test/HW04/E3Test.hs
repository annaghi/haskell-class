module HW04.E3Test (tests) where

import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified HW04.E3 as EE


tests :: TestTree
tests =
    testGroup "Exercise 3"
        [ testProperty "xor" $
            property $ do
                list <- forAll genBools
                let
                    helper :: Int -> Bool
                    helper n =
                        if n == 0 then
                            False
                        else
                            (/= 0) $ n `mod` 2
                EE.xor list === (helper $ length $ filter (== True) list)
        , testProperty "map'" $
            property $ do
                list <- forAll genBools
                EE.map' not list === map not list
        , testProperty "myFoldl" $
            property $ do
                list <- forAll genBools
                EE.myFoldl (\a e -> e : a) [] list === foldl (\a e -> e : a) [] list
        ]



-- Generators


genBools :: MonadGen m => m [Bool]
genBools =
    Gen.list (Range.linear 0 10) Gen.bool_
