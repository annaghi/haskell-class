module RWH.P098Test (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Hedgehog (MonadGen, assert, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.List (group)

import qualified RWH.P098 as EE


tests :: TestTree
tests =
    testGroup "Page 98"
        [ testGroup "Exercise 6"
            [ testProperty "concatFoldr" $
                property $ do
                    xs <- forAll genStrings
                    EE.concatFoldr xs === concat xs
            ]
        , testGroup "Exercise 7"
            [ testProperty "takeWhileRecursive" $
                property $ do
                    xs <- forAll genStrings
                    EE.takeWhileRecursive (not . null) xs === takeWhile (not . null) xs
            , testProperty "takeWhileFoldr" $
                property $ do
                    xs <- forAll genStrings
                    EE.takeWhileFoldr (not . null) xs === takeWhile (not . null) xs
            ]
        , testGroup "Exercise 8"
            [ testProperty "groupByRecursive" $
                property $ do
                    xs <- forAll genString
                    EE.groupByRecursive (==) xs === group xs
            ]
        , testGroup "Exercise 9"
            [ testProperty "groupByFoldr" $
                property $ do
                    xs <- forAll genString
                    EE.groupByFoldr (==) xs === group xs
            , testProperty "groupByFoldr2" $
                property $ do
                    xs <- forAll genString
                    EE.groupByFoldr2 (==) xs === group xs
            , testProperty "groupByFoldl" $
                property $ do
                    xs <- forAll genString
                    EE.groupByFoldl (==) xs === group xs
            ]
        , testGroup "Exercise 10"
            [ testProperty "anyFoldr" $
                property $ do
                    xs <- forAll genString
                    EE.anyFoldr (== 'a') xs === elem 'a' xs
            , testProperty "cycleFoldr" $
                property $ do
                    xs <- forAll genString
                    if null xs then
                        assert $ null (EE.cycleFoldr xs)
                    else
                        take 10 (EE.cycleFoldr xs) === take 10 (cycle xs)
            , testProperty "wordsFoldr" $
                property $ do
                    xs <- forAll genString
                    EE.wordsFoldr xs === words xs
            , testProperty "unlinesFoldr" $
                property $ do
                    xs <- forAll genStrings
                    EE.unlinesFoldr xs === unlines xs
            ]
        ]




-- Generators


genString :: MonadGen m => m String
genString =
    Gen.list (Range.linear 0 100) Gen.alpha


genStrings :: MonadGen m => m [String]
genStrings =
    Gen.list (Range.linear 0 20) genString
