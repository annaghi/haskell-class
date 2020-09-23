module HW03.SkipsTest (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Utils ((|>))

import qualified HW03.Golf as EE


tests :: TestTree
tests =
    testGroup "Exercise 1: Hopscotch"
        [ testCase "when the list is empty" $
            EE.skips ([] :: [Int])
                |> assertEqual "" []
        , testCase "when the list has one element" $
            EE.skips [1]
                |> assertEqual "" [[1]]
        , testCase "when the list is \"ABCD\"" $
            EE.skips "ABCD"
                |> assertEqual "" ["ABCD", "BD", "C", "D"]
        , testCase "when the list is \"hello!\"" $
            EE.skips "hello!"
                |> assertEqual "" ["hello!", "el!", "l!", "l", "o", "!"]
        , testCase "when the list is [True,False]" $
            EE.skips [True,False]
                |> assertEqual "" [[True,False], [False]]
        ]
