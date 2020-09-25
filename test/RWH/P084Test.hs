module RWH.P084Test (tests) where

import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.List.Safe as Safe
import Data.List.Split (wordsBy)

import qualified RWH.P084 as EE


tests :: TestTree
tests =
    testGroup "Page 84"
        [ testGroup "Exercise 1"
            [ testProperty "safeHead" $
                property $ do
                    xs <- forAll genString
                    EE.safeHead xs === Safe.head xs
            , testProperty "safeTail" $
                property $ do
                    xs <- forAll genString
                    EE.safeTail xs === Safe.tail xs
            , testProperty "safeInit" $
                property $ do
                    xs <- forAll genString
                    EE.safeInit xs === Safe.init xs
            , testProperty "safeLast" $
                property $ do
                    xs <- forAll genString
                    EE.safeLast xs === Safe.last xs
            , testProperty "splitWith" $
                property $ do
                    xs <- forAll genString
                    EE.splitWith (== 'a') xs === wordsBy (== 'a') xs
            ]
        ]


-- Generators


genString :: MonadGen m => m String
genString =
    Gen.list (Range.linear 0 4) Gen.alpha
