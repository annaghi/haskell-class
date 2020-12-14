module RWH.P084Test (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Utils ((|>))
import Test.Tasty.Hedgehog (testProperty)
import Hedgehog (MonadGen, forAll, property, (===))
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
             , testCase "firstWords: when the list is empty" $
                EE.firstWords ""
                    |> assertEqual "" ""
            , testCase "firstWords: when the list contains a new line character" $
                EE.firstWords "\n"
                    |> assertEqual "" ""
            , testCase "firstWords: when the list contains a single digit" $
                EE.firstWords "a"
                    |> assertEqual "" "a"
            , testProperty "firstWords: when using the helper function firstWords'" $
                property $ do
                    xs <- forAll genText
                    (length . EE.firstWords') xs === (length . filter (== '\n') . compress) xs
            ]
        ]



-- Generators


genString :: MonadGen m => m String
genString =
    Gen.list (Range.linear 0 4) Gen.alpha


genText :: MonadGen m => m String
genText =
    fmap (dropWhile (== '\n') . concatMap (reverse . ('\n' :)))
    $ Gen.list (Range.linear 0 10) genString



-- Helpers


compress []     = []
compress (x:xs) = x : compress (dropWhile (== x) xs)
